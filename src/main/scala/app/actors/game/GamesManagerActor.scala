package app.actors.game

import java.util.UUID

import akka.event.Logging
import akka.typed.ScalaDSL._
import akka.typed._
import app.actors.NetClient.LoggedInState.JoinGame.{PvPMode, Mode}
import app.actors.game.GameActor.{ClientData, StartingHuman}
import app.actors.{GCMSender, NetClient}
import app.models.User
import app.models.game.world.maps.{GameMaps, SingleplayerMap, WorldMaterializer}
import app.models.game.world.{ExtractorStats, Resources, World}
import app.models.game.{Bot, Human, Team, TurnTimers}
import implicits._, implicits.actor._
import infrastructure.GCM
import launch.RTConfig
import org.joda.time.DateTime
import spire.math.UInt

import scala.concurrent.duration._
import scalaz.Scalaz._
import scalaz.effect.IO

object GamesManagerActor {
  type Ref = ActorRef[In]

  val StartingResources = ExtractorStats.cost * Resources(4)

  sealed trait Message

  sealed trait In extends Message
  object In {
    case class FromNetClient(msg: NetClient.GamesManagerFwd) extends In

    // After user connects to the server, he should check whether he is in game or not.
    case class CheckUserStatus(user: User, client: NetClient.LoggedInRef) extends In

    // Game joining
    case class Join(
      user: User, mode: NetClient.LoggedInState.JoinGame.Mode,
      replyTo: NetClient.LoggedInRef
    ) extends In
    case class CancelJoinGame(
      user: User, replyTo: ActorRef[NetClient.LoggedInState.JoinGameCancelled.type]
    ) extends In

    // Stats report for control client
    case class StatsReport(replyTo: ActorRef[StatsReportData]) extends In

    case object ShutdownInitiated extends In
  }

  private[this] sealed trait Internal extends Message
  private[this] object Internal {
    case object CleanupBackgroundWaitingList extends Internal
    /* Check if we can shutdown. */
    case object CheckShutdown extends Internal

    case class GameTerminated(ref: GameActor.Ref) extends Internal
    case class ClientTerminated(ref: NetClient.LoggedInRef) extends Internal
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

  case class StatsReportData(users: UInt, games: UInt)

  case class BackgroundToken(value: String) extends AnyVal
  object BackgroundToken {
    val newToken = IO { BackgroundToken(UUID.randomUUID().toString) }
  }

  case class WaitingListEntry(
    user: User, client: NetClient.LoggedInRef,
    backgroundToken: BackgroundToken
  )

  private def joinGame(
    game: GameActor.Ref, human: Human, client: NetClient.LoggedInRef
  ): Unit = game ! GameActor.In.Join(ClientData(human, client), client)

  def behaviour(
    maps: GameMaps, gcm: Option[(ActorRef[GCMSender.Send], RTConfig.GCM)]
  )(implicit rtConfig: RTConfig): Behavior[In] = ContextAware[Message] { ctx =>
    val log = ctx.createLogging()

    def scheduleCleanup(): Unit =
      ctx.schedule(1.second, ctx.self, Internal.CleanupBackgroundWaitingList)

    def scheduleShutdownMode(): Unit =
      ctx.schedule(1.second, ctx.self, Internal.CheckShutdown)

    var waitingList = Vector.empty[WaitingListEntry]
    // token -> last heartbeat
    var waitingInBackground = Map.empty[BackgroundToken, DateTime]
    var user2game = Map.empty[User, (GameActor.Ref, Human)]
    var game2humans = Map.empty[GameActor.Ref, Set[Human]]

    def removeBackgroundToken(token: BackgroundToken): Unit = {
      log.info("Removing background token: {}", token)
      waitingInBackground -= token
      notifyGCM()
    }

    def notifyGCM(): Unit = {
      gcm.foreach { case (ref, cfg) =>
        val foreground = GCM.Data.SearchingForOpponent.InForeground(UInt(waitingList.size))
        val background = GCM.Data.SearchingForOpponent.InBackground(UInt(waitingInBackground.size))
        ref ! GCMSender.Send(
          GCM.searchingForOpponent(foreground, background, cfg.searchForOpponentTTL)
        )
      }
    }

    def noExistingGame(
      user: User, mode: Mode, client: NetClient.LoggedInRef
    ): Unit = {
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
            ctx.watchWith(client, Internal.ClientTerminated(client))
            client ! NetClient.LoggedInState.WaitingListJoined(token)
          }
          else fromWaitingList(pvp)
      }
    }

    def launchPVE(user: User, client: NetClient.LoggedInRef) = {
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
        Set(StartingHuman(Human(user, Team()), StartingResources, client, client))
      )
    }

//    def launchRandomGenerated(user: User, client: NetClient.LoggedInRef) = {
//      val materializer = SingleplayerMap { data => implicit log =>
//        val npcTeam = Team()
//        val npcBot = Bot(npcTeam)
//        val spawnerBot = Bot(npcTeam)
//        World.create(
//          data.humanTeam, () => npcBot, () => spawnerBot, staticObjectsKnownAtStart = false
//        )
//      }
//      createGame(
//        materializer, None, Team(),
//        Set(StartingHuman(Human(user, Team()), StartingResources, client, client))
//      )
//    }

    def fromWaitingList(mode: PvPMode): Unit = {
      val (entries, newWaitingList) = waitingList.splitAt(mode.playersNeeded)
      waitingList = newWaitingList
      notifyGCM()

      val teams = Vector.fill(mode.teams)(Team())
      val players = entries.zipWithIndex.map { case (entry, idx) =>
        val team = teams.wrapped(idx)
        StartingHuman(
          Human(entry.user, team), StartingResources, entry.client, entry.client
        )
      }.toSet

      log.debug(
        "Fetched {} from waiting list for mode {}, rest={}", players, mode, newWaitingList
      )
      // TODO: will fail if we have more teams than any of the maps support
      val map = maps.pvpMapFor(mode.playersNeeded).right_!.unsafePerformIO()
      val npcTeam = Team()

      createGame(map, Some(TurnTimers.Settings()), npcTeam, players)
    }

    def createGame(
      worldMaterializer: WorldMaterializer, turnTimerSettings: Option[TurnTimers.Settings],
      npcTeam: Team, starting: Set[GameActor.StartingHuman]
    ): GameActor.Ref = {
      val game = ctx.spawnAnonymous(Props(GameActor.behavior(
        worldMaterializer, turnTimerSettings, npcTeam, starting
      )))
      ctx.watchWith(game, Internal.GameTerminated(game))
      starting.foreach { data =>
        user2game += data.human.user -> ((game, data.human))
      }
      game2humans += game -> starting.map(_.human)
      log.info("Game {} created for {}", game, starting)
      game
    }

    scheduleCleanup()
    Total[Message] {
      case Internal.CleanupBackgroundWaitingList =>
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

        scheduleCleanup()
        Same

      case In.ShutdownInitiated =>
        log.info("Shutdown mode initiated.")
        scheduleShutdownMode()
        Same

      case Internal.CheckShutdown =>
        val games = game2humans.size
        log.debug("Checking for shutdown state, games: {}", games)
        if (games === 0) {
          log.info("No games alive, shutting down.")
          ctx.system.terminate()
          Stopped
        }
        else {
          scheduleShutdownMode()
          Same
        }

      case In.CheckUserStatus(user, client) =>
        user2game.get(user).foreach { case (game, human) =>
          log.info("{} joining game {} on user status check", human, game)
          joinGame(game, human, client)
        }
        Same

      case In.Join(user, mode, replyTo) =>
        user2game.get(user).fold2(
          {
            if (waitingList.exists(_.user === user)) log.warning(
              "Not joining a new game, because {} is already in a waiting list, ref: {}",
              user, replyTo
            )
            else noExistingGame(user, mode, replyTo)
          },
          { case (game, human) =>
            log.info("{} joining game {} on game join", human, game)
            joinGame(game, human, replyTo)
          }
        )
        Same

      case In.CancelJoinGame(user, replyTo) =>
        waitingList.indexWhere(_.user === user) match {
          case -1 =>
            log.warning("Not cancelling join game, because {} is not in a waiting list.", user)
          case idx =>
            val entry = waitingList(idx)
            ctx.unwatch(entry.client)
            waitingList = waitingList.removeAt(idx)
            notifyGCM()
            replyTo ! NetClient.LoggedInState.JoinGameCancelled
        }
        Same

      case In.StatsReport(replyTo) =>
        replyTo ! StatsReportData(UInt(user2game.size), UInt(game2humans.size))
        Same

      case In.FromNetClient(NetClient.NotLoggedInState.CancelBackgroundToken(token)) =>
        removeBackgroundToken(token)
        Same

      case In.FromNetClient(NetClient.MsgHandlerConnectionIn.BackgroundSFO(kind, token)) =>
        if (waitingInBackground contains token) {
          kind match {
            case NetClient.MsgHandlerConnectionIn.BackgroundSFO.Kind.Heartbeat =>
              waitingInBackground += token -> DateTime.now()
              log.debug("Background heartbeat from {}", token)
            case NetClient.MsgHandlerConnectionIn.BackgroundSFO.Kind.Cancel =>
              removeBackgroundToken(token)
          }
        }
        else {
          // TODO: should we tell sender that his heartbeat was expired?
          log.info("Ignoring background {} from unknown token: {}", kind, token)
        }
        Same

      case Internal.ClientTerminated(ref) =>
        waitingList.zipWithIndex.collectFirst {
          case (entry @ WaitingListEntry(_, `ref`, _), idx) =>
            (entry, idx)
        }.foreach { case (entry, idx) =>
          log.info("{} going into background", entry)
          waitingList = waitingList.removeAt(idx)
          waitingInBackground += entry.backgroundToken -> DateTime.now()
          notifyGCM()
        }
        Same

      case Internal.GameTerminated(ref) =>
        game2humans.get(ref) match {
          case Some(humans) =>
            log.info("Game {} terminated for humans {}", ref, humans)
            game2humans -= ref
            humans.foreach { human => user2game -= human.user }
          case None =>
            log.warning(
              "Game {} terminated, but can't find it in our state!", ref
            )
        }
        Same
    }
  }.narrow
}
