package app.actors.game

import java.util.UUID

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.event.{LoggingAdapter, LoggingReceive}
import app.actors.MsgHandler.Client2Server.BackgroundSFO
import app.actors.NetClient.Management.In.JoinGame.{Mode, PvPMode}
import app.actors.game.GameActor.StartingHuman
import app.actors.{MsgHandler, NetClient, Server}
import app.models.User
import app.models.game.world.maps.{GameMaps, SingleplayerMap, WorldMaterializer}
import app.models.game.world.{ExtractorStats, Resources, World}
import app.models.game.{Bot, Human, Team, TurnTimers}
import implicits._
import infrastructure.GCM
import launch.RTConfig
import org.joda.time.DateTime
import spire.math.UInt

import scala.concurrent.duration._
import scalaz.Scalaz._
import scalaz._
import scalaz.effect.IO

object GamesManagerActor {
  val StartingResources = ExtractorStats.cost * Resources(4)

  sealed trait In
  object In {
    // After user connects to the server, he should check whether he is in game or not.
    case class CheckUserStatus(user: User) extends In

    // Game joining
    case class Join(user: User, mode: NetClient.Management.In.JoinGame.Mode) extends In
    case class CancelJoinGame(user: User) extends In

    // Stats report for control client
    case object StatsReport extends In
  }

  sealed trait Out
  object Out {
    case class StatsReport(users: UInt, games: UInt) extends Out
  }

  sealed trait Internal
  object Internal {
    case object CleanupBackgroundWaitingList
    /* Check if we can shutdown. */
    case object CheckShutdown
  }

  // TODO: proper singleplayer
//  object PVEGame {
//    sealed trait PresetTeam {
//      def gameTeam: Team
//    }
//    object PresetTeam {
//      object Red extends PresetTeam { val gameTeam = Team() }
//      object Blue extends PresetTeam { val gameTeam = Team() }
//    }
//
//    val empty = PVEGame(None, Set.empty, Set.empty)
//  }
//  case class PVEGame(ref: Option[ActorRef], redTeamPlayers: Set[User], blueTeamPlayers: Set[User]) {
//    def giveTeam: PVEGame.PresetTeam =
//      redTeamPlayers.size ?|? blueTeamPlayers.size match {
//        case Ordering.LT => PVEGame.PresetTeam.Red
//        case Ordering.GT => PVEGame.PresetTeam.Blue
//        case Ordering.EQ => if (Random.chance(0.5)) PVEGame.PresetTeam.Red else PVEGame.PresetTeam.Blue
//      }
//
//    def add(user: User, team: PresetTeam): PVEGame = team match {
//      case PresetTeam.Red => copy(redTeamPlayers = redTeamPlayers + user)
//      case PresetTeam.Blue => copy(blueTeamPlayers = blueTeamPlayers + user)
//    }
//  }

  case class BackgroundToken(value: String) extends AnyVal
  object BackgroundToken {
    val newToken = IO { BackgroundToken(UUID.randomUUID().toString) }
  }

  case class WaitingListEntry(user: User, client: ActorRef, backgroundToken: BackgroundToken)

  private def joinGame(game: ActorRef, human: Human, client: ActorRef): Unit =
    game.tell(GameActor.In.Join(human), client)
}

class GamesManagerActor(
  maps: GameMaps, gcm: Option[(ActorRef, RTConfig.GCM)]
)(implicit rtConfig: RTConfig) extends Actor with ActorLogging {
  import app.actors.game.GamesManagerActor._
  import context.dispatcher

  private[this] var waitingList = Vector.empty[WaitingListEntry]
  // token -> last heartbeat
  private[this] var waitingInBackground = Map.empty[BackgroundToken, DateTime]
  private[this] var user2game = Map.empty[User, (ActorRef, Human)]
  private[this] var game2humans = Map.empty[ActorRef, Set[Human]]

  context.system.scheduler.schedule(
    0.seconds, 1.second, self, GamesManagerActor.Internal.CleanupBackgroundWaitingList
  )

  override def supervisorStrategy = OneForOneStrategy() {
    case _ => Stop
  }

  private[this] val notLoggedReceive: Receive = {
    case GamesManagerActor.Internal.CleanupBackgroundWaitingList =>
      val now = DateTime.now()
      val expiredKeys = waitingInBackground.keys.filter { token =>
        val lastBeat = waitingInBackground(token)
        val timePassed = now - lastBeat
        val active = timePassed <= rtConfig.gamesManager.backgroundHeartbeatTTL.duration
        if (! active) log.debug(
          "Timing out background token {}: {} > {}",
          token, timePassed, rtConfig.gamesManager.backgroundHeartbeatTTL.duration
        )
        !active
      }
      expiredKeys.foreach(waitingInBackground -= _)
      if (expiredKeys.nonEmpty) notifyGCM()

    case GamesManagerActor.Internal.CheckShutdown =>
      val games = game2humans.size
      log.debug("Checking for shutdown state, games: {}", games)
      if (games === 0) {
        log.info("No games alive, shutting down.")
        context.system.terminate()
      }
  }

  override def receive = notLoggedReceive orElse LoggingReceive {
    case GamesManagerActor.In.CheckUserStatus(user) =>
      user2game.get(user).foreach { case (game, human) =>
        log.info("{} joining game {} on user status check", human, game)
        joinGame(game, human, sender())
      }

    case GamesManagerActor.In.Join(user, mode) =>
      user2game.get(user).fold2(
        {
          if (waitingList.exists(_.user === user)) log.warning(
            "Not joining a new game, because {} is already in a waiting list, ref: {}",
            user, sender()
          )
          else noExistingGame(user, mode, sender())
        },
        { case (game, human) =>
          log.info("{} joining game {} on game join", human, game)
          joinGame(game, human, sender())
        }
      )

    case GamesManagerActor.In.CancelJoinGame(user) =>
      waitingList.indexWhere(_.user === user) match {
        case -1 =>
          log.warning("Not cancelling join game, because {} is not in a waiting list.", user)
        case idx =>
          val entry = waitingList(idx)
          context.unwatch(entry.client)
          waitingList = waitingList.removeAt(idx)
          notifyGCM()
          sender() ! NetClient.Management.Out.JoinGameCancelled
      }

    case NetClient.Management.In.CancelBackgroundToken(token) =>
      removeBackgroundToken(token)

    case MsgHandler.Client2Server.BackgroundSFO(kind, token) =>
      if (waitingInBackground contains token) {
        kind match {
          case BackgroundSFO.Kind.Heartbeat =>
            waitingInBackground += token -> DateTime.now()
            log.debug("Background heartbeat from {}", token)
          case BackgroundSFO.Kind.Cancel =>
            removeBackgroundToken(token)
        }
      }
      else {
        // TODO: should we tell sender that his heartbeat was expired?
        log.info("Ignoring background {} from unknown token: {}", kind, token)
      }

    case Terminated(ref) =>
      // Game termination
      game2humans.get(ref).foreach { humans =>
        log.info("Game {} terminated for humans {}", ref, humans)
        game2humans -= ref
        humans.foreach { human => user2game -= human.user }
      }

      // NetClient termination
      waitingList.zipWithIndex.collectFirst {
        case (entry @ WaitingListEntry(_, `ref`, _), idx) =>
          (entry, idx)
      }.foreach { case (entry, idx) =>
        log.info("{} going into background", entry)
        waitingList = waitingList.removeAt(idx)
        waitingInBackground += entry.backgroundToken -> DateTime.now()
        notifyGCM()
      }

    case Server.ShutdownInitiated =>
      log.info("Shutdown mode initiated.")
      context.system.scheduler
        .schedule(0.seconds, 1.second, self, GamesManagerActor.Internal.CheckShutdown)

    case GamesManagerActor.In.StatsReport =>
      sender ! GamesManagerActor.Out.StatsReport(UInt(user2game.size), UInt(game2humans.size))
  }

  private[this] def removeBackgroundToken(token: BackgroundToken): Unit = {
    log.info("Removing background token: {}", token)
    waitingInBackground -= token
    notifyGCM()
  }

  private[this] def noExistingGame(user: User, mode: Mode, client: ActorRef): Unit = {
    mode match {
      case Mode.Singleplayer =>
        //launchRandomGenerated(user, client)
        launchPVE(user, client)
      case pvp: PvPMode =>
        val token = BackgroundToken.newToken.unsafePerformIO()
        val entry = WaitingListEntry(user, client, token)
        waitingList :+= entry
        if (waitingList.size < pvp.playersNeeded) {
          log.debug(
            "Added {} from {} to {} waiting list: {}",
            user, client, mode, waitingList
          )
          notifyGCM()
          context.watch(client)
          client ! NetClient.Management.Out.WaitingListJoined(token)
        }
        else fromWaitingList(pvp)
    }
  }

  private[this] def notifyGCM(): Unit = {
    gcm.foreach { case (ref, cfg) =>
      val foreground = GCM.Data.SearchingForOpponent.InForeground(UInt(waitingList.size))
      val background = GCM.Data.SearchingForOpponent.InBackground(UInt(waitingInBackground.size))
      ref ! GCM.searchingForOpponent(foreground, background, cfg.searchForOpponentTTL)
    }
  }

  private[this] def launchPVE(user: User, client: ActorRef) = {
    // TODO: proper PVE
//    val team = pveGame.giveTeam
//    if (pveGame.ref.isEmpty) {
//      val game = createGame(
//        maps.pve.random, Some(TurnTimers.Settings()), Team(),
//        Set(StartingHuman(Human(user, team.gameTeam), StartingResources, client))
//      )
//      pveGame = pveGame.copy(ref = Some(game))
//    }
//    pveGame = pveGame.add(user, team)

    createGame(
      maps.pve.random, None, Team(),
      Set(StartingHuman(Human(user, Team()), StartingResources, client))
    )
  }

  private[this] def launchRandomGenerated(user: User, client: ActorRef) = {
    val materializer = SingleplayerMap { data => implicit log =>
      val npcTeam = Team()
      val npcBot = Bot(npcTeam)
      val spawnerBot = Bot(npcTeam)
      World.create(
        data.humanTeam, () => npcBot, () => spawnerBot, staticObjectsKnownAtStart = false
      )
    }
    createGame(
      materializer, None, Team(),
      Set(StartingHuman(Human(user, Team()), StartingResources, client))
    )
  }

  private[this] def fromWaitingList(mode: PvPMode): Unit = {
    val (entries, newWaitingList) = waitingList.splitAt(mode.playersNeeded)
    waitingList = newWaitingList
    notifyGCM()

    val teams = Vector.fill(mode.teams)(Team())
    val players = entries.zipWithIndex.map { case (entry, idx) =>
      val team = teams.wrapped(idx)
      StartingHuman(Human(entry.user, team), StartingResources, entry.client)
    }.toSet

    log.debug(
      "Fetched {} from waiting list for mode {}, rest={}", players, mode, newWaitingList
    )
    // TODO: will fail if we have more teams than any of the maps support
    val map = maps.pvpMapFor(mode.playersNeeded).right_!.unsafePerformIO()
    val npcTeam = Team()

    createGame(map, Some(TurnTimers.Settings()), npcTeam, players)
  }

  private[this] def createGame(
    worldMaterializer: WorldMaterializer, turnTimerSettings: Option[TurnTimers.Settings],
    npcTeam: Team, starting: Set[GameActor.StartingHuman]
  ): ActorRef = {
    val game = context.actorOf(GameActor.props(
      worldMaterializer, turnTimerSettings, npcTeam, starting
    ))
    context.watch(game)
    starting.foreach { data =>
      user2game += data.human.user -> ((game, data.human))
    }
    game2humans += game -> starting.map(_.human)
    log.info("Game {} created for {}", game, starting)
    game
  }
}
