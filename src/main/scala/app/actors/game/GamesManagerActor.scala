package app.actors.game

import java.util.UUID

import akka.actor.{Scheduler, Cancellable}
import akka.event.Logging
import akka.typed.ScalaDSL._
import akka.typed._
import app.actors.NetClient.LoggedInState.JoinGame.{PvPMode, Mode}
import app.actors.game.GameActor.{ClientData, StartingHuman}
import app.actors.game.GamesManagerActor.BackgroundNotConnected
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
import scalaz.{-\/, \/-, \/}
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

    /* Sent before Unity goes to background. */
    case class ClientGoingToBackground(user: User) extends In
    /* Sent when background client connects to the server. */
    case class BackgroundClientConnected(
      token: BackgroundToken, replyTo: ActorRef[NetClient.NotLoggedInState.BackgroundLoginReply],
      ref: NetClient.BGClientRef
    ) extends In
    /* Sent when "Leave Queue" is pressed in background client. */
    case class BackgroundClientLeaveQueue(token: BackgroundToken) extends In

    // Stats report for control client
    case class StatsReport(replyTo: ActorRef[StatsReportData]) extends In

    case object ShutdownInitiated extends In
  }

  private[this] sealed trait Internal extends Message
  private[this] object Internal {
    case class BGTokenExpired(token: BackgroundToken) extends Internal

    case class GameTerminated(ref: GameActor.Ref) extends Internal
    case class ClientTerminated(ref: NetClient.LoggedInRef) extends Internal
    case class BackgroundClientTerminated(token: BackgroundToken) extends Internal
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
    user: User, client: NetClient.LoggedInRef, backgroundToken: BackgroundToken
  ) {
    def toBackground(now: DateTime, schedule: FiniteDuration => IO[Cancellable])
        (implicit rtConfig: RTConfig) =
      BackgroundNotConnected.apply(now, schedule).map { state =>
        BackgroundWaitingListEntry(user, state.left)
      }
  }

  case class BackgroundNotConnected(lastSeen: DateTime, scheduledCheck: Cancellable)
  object BackgroundNotConnected {
    def apply(now: DateTime, schedule: FiniteDuration => IO[Cancellable])(implicit rtConfig: RTConfig)
    : IO[BackgroundNotConnected] = for {
      scheduled <- schedule(rtConfig.gamesManager.backgroundHeartbeatTTL.duration)
    } yield apply(now, scheduled)
  }

  case class BackgroundWaitingListEntry(
    user: User, infoOrConn: BackgroundNotConnected \/ NetClient.BGClientRef
  ) {
    def cancelScheduledCheck = IO { infoOrConn.swap.foreach(_.scheduledCheck.cancel()) }
  }

  private def joinGame(
    game: GameActor.Ref, human: Human, client: NetClient.LoggedInRef
  ): Unit = game ! GameActor.In.Join(ClientData(human, client), client)

  def behaviour(
    maps: GameMaps, gcm: Option[ActorRef[GCMSender.Send]]
  )(implicit rtConfig: RTConfig): Behavior[In] = ContextAware[Message] { ctx =>
    val log = ctx.createLogging()

    var waitingList = Vector.empty[WaitingListEntry]
    var waitingInBackground = Map.empty[BackgroundToken, BackgroundWaitingListEntry]
    var user2game = Map.empty[User, (GameActor.Ref, Human)]
    var game2humans = Map.empty[GameActor.Ref, Set[Human]]
    var shuttingDown = false

    def findUserInWaitingListF(f: WaitingListEntry => Boolean) = waitingList.indexWhere(f) match {
      case -1 => None
      case idx => Some((waitingList(idx), idx))
    }
    def findUserInWaitingList(user: User) = findUserInWaitingListF(_.user === user)

    def removeBackgroundToken(token: BackgroundToken): Unit = {
      waitingInBackground.get(token).fold2(
        log.info("Background token {} not found for removal.", token),
        entry => {
          log.info("Removing background token: {}", token)
          entry.cancelScheduledCheck.unsafePerformIO()
          waitingInBackground -= token
          notifyWaitingListChanged()
        }
      )
    }

    def waitingListIds = waitingList.map(_.user.id).toSet ++ waitingInBackground.map(_._2.user.id)

    def notifyWaitingListChanged(): Unit = {
      val gcmWaitingList = waitingListIds
      gcm.foreach { _ ! GCMSender.Send(GCM.SearchingForOpponent(gcmWaitingList).message) }
      waitingInBackground.foreach {
        case (_, BackgroundWaitingListEntry(user, \/-(bgRef))) =>
          val opponentWaiting = !(
            gcmWaitingList.isEmpty ||
            (gcmWaitingList.size == 1 && gcmWaitingList.contains(user.id))
          )
          bgRef ! NetClient.BackgroundLoggedInState.WaitingListChanged(opponentWaiting)
        case _ =>
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
            notifyWaitingListChanged()
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
      notifyWaitingListChanged()

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
        ctx.unwatch(data.client)
      }
      game2humans += game -> starting.map(_.human)
      log.info("Game {} created for {}", game, starting)
      game
    }

    def tryToShutdown(): Behavior[Message] = {
      val games = game2humans.size
      log.debug("Checking for shutdown state, games: {}", games)
      if (games === 0) {
        log.info("No games alive, shutting down.")
        ctx.system.terminate()
        Stopped
      }
      else Same
    }

    def scheduleBGTokenExpired(token: BackgroundToken)(delay: FiniteDuration) =
      IO { ctx.schedule(delay, ctx.self, Internal.BGTokenExpired(token)) }

    Total[Message] {
      case In.CheckUserStatus(user, client) =>
        user2game.get(user).foreach { case (game, human) =>
          log.info("{} joining game {} on user status check", human, game)
          joinGame(game, human, client)
        }
        Same
        
      // <editor-fold desc="Game joining">

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
        findUserInWaitingList(user).fold2(
          log.warning("Not cancelling join game, because {} is not in a waiting list.", user),
          { case (entry, idx) =>
            ctx.unwatch(entry.client)
            waitingList = waitingList.removeAt(idx)
            notifyWaitingListChanged()
            replyTo ! NetClient.LoggedInState.JoinGameCancelled
          }
        )
        Same

      case msg @ In.ClientGoingToBackground(user) =>
        findUserInWaitingList(user) match {
          case None =>
            log.error("Cannot find user {} in the waiting list for {}", user, msg)
          case Some((entry, idx)) =>
            waitingList = waitingList.removeAt(idx)
            waitingInBackground += entry.backgroundToken -> entry.toBackground(
              DateTime.now(),
              scheduleBGTokenExpired(entry.backgroundToken)
            ).unsafePerformIO()
            ctx.unwatch(entry.client)
        }
        Same

      case Internal.ClientTerminated(ref) =>
        log.info("client terminated: {}", ref)
        findUserInWaitingListF(_.client === ref).foreach { case (entry, idx) =>
          waitingList = waitingList.removeAt(idx)
          notifyWaitingListChanged()
        }
        Same

      // </editor-fold>

      // <editor-fold desc="Background client handling">

      case In.BackgroundClientConnected(token, replyTo, ref) =>
        waitingInBackground.get(token).fold2(
          {
            log.info("Unknown background client {} connected with token {}", ref, token)
            replyTo ! NetClient.NotLoggedInState.BackgroundLoginReply("unknown token".left)
          },
          entry => {
            log.info("Background client {} connected with token {}", ref, token)
            entry.cancelScheduledCheck.unsafePerformIO()
            waitingInBackground += token -> entry.copy(infoOrConn = ref.right)
            ctx.watchWith(ref, Internal.BackgroundClientTerminated(token))
            replyTo ! NetClient.NotLoggedInState.BackgroundLoginReply(token.right)
          }
        )
        Same

      case Internal.BackgroundClientTerminated(token) =>
        waitingInBackground.get(token).fold2(
          log.error("Can't find background client by token {} on termination!", token),
          entry => {
            log.info("background client terminated unvoluntary: {} -> {}", token, entry)
            val state = (for {
              _ <- entry.cancelScheduledCheck
              state <- BackgroundNotConnected(DateTime.now(), scheduleBGTokenExpired(token) _)
            } yield state).unsafePerformIO()
            waitingInBackground += token -> entry.copy(infoOrConn = state.left)
          }
        )
        Same
        
      case In.BackgroundClientLeaveQueue(token) =>
        log.info("background client leaving queue: {}", token)
        removeBackgroundToken(token)
        Same

      case Internal.BGTokenExpired(token) =>
        waitingInBackground.get(token) match {
          case None =>
            log.debug("No BG token to expire: {}", token)
          case Some(BackgroundWaitingListEntry(_, -\/(_))) =>
            log.info("Timing out background token {}", token)
            removeBackgroundToken(token)
          case _ =>
            log.debug("Client with BG token {}")
        }
        Same

      case In.FromNetClient(NetClient.NotLoggedInState.CancelBackgroundToken(token)) =>
        log.info("logged in client is cancelling his background token: {}", token)
        removeBackgroundToken(token)
        Same

      // </editor-fold>

      // <editor-fold desc="Game tracking">

      case Internal.GameTerminated(ref) =>
        log.info("game terminated: {}", ref)
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

        if (shuttingDown) tryToShutdown()
        else Same

      // </editor-fold>

      // <editor-fold desc="Shutdown procedure">

      case In.ShutdownInitiated =>
        log.info("Shutdown mode initiated.")
        shuttingDown = true
        tryToShutdown()

      // </editor-fold>

      // <editor-fold desc="Stats">

      case In.StatsReport(replyTo) =>
        replyTo ! StatsReportData(UInt(user2game.size), UInt(game2humans.size))
        Same

      // </editor-fold>

    }
  }.narrow
}
