package app.actors.game

import akka.event.LoggingAdapter
import akka.typed.ScalaDSL._
import akka.typed._
import app.actors.NetClient
import app.models.game._
import app.models.game.events._
import app.models.game.world.WObject.Id
import app.models.game.world._
import app.models.game.world.maps.WorldMaterializer
import app.models.game.world.props.ExtractionSpeed
import implicits._
import implicits.actor._
import org.joda.time.DateTime
import utils.data.{NonEmptyVector, Timeframe}

import scala.concurrent.duration._
import scala.language.{existentials, implicitConversions}
import scalaz.Scalaz._
import scalaz._

object GameActor {
  type Ref = akka.typed.ActorRef[In]
  private[this] type NetClientJoinedRef = ActorRef[NetClient.LoggedInState.GameJoined]
  type NetClientOutRef = ActorRef[NetClient.InGameState.FromGameActor]

  sealed trait Message

  case class ClientData(human: Human, replyTo: NetClientOutRef)

  sealed trait In extends Message
  object In {
    case class Join(
      clientData: ClientData, replyJoined: NetClientJoinedRef
    ) extends In
    case class Warp(
      clientData: ClientData, position: Vect2, warpable: WarpableCompanion.Some
    ) extends In
    /* path does not include objects position and ends in target position */
    case class Move(
      clientData: ClientData, id: WObject.Id, path: NonEmptyVector[Vect2]
    ) extends In
    case class Attack(
      clientData: ClientData, id: WObject.Id, target: Vect2 \/ WObject.Id
    ) extends In
    case class MoveAttack(
      move: Move, target: Vect2 \/ WObject.Id
    ) extends In
    case class Special(
      clientData: ClientData, id: WObject.Id
    ) extends In
    case class ToggleWaitingForRoundEnd(clientData: ClientData) extends In
    case class Concede(clientData: ClientData) extends In
  }

  private[this] sealed trait Internal extends Message
  private[this] object Internal {
    case object CheckTurnTime extends Internal
  }

  sealed trait NetClientOut
  implicit def asNetClient(msg: NetClientOut): NetClient.InGameState.FromGameActor =
    NetClient.InGameState.FromGameActor(msg)

  object NetClientOut {
    case class Init(
      id: World.Id, bounds: Bounds, objects: WorldObjs.All,
      warpZonePoints: Iterable[Vect2], visiblePoints: Iterable[Vect2],
      selfTeam: Team, otherTeams: Iterable[Team],
      self: HumanState, others: Iterable[(Player, Option[HumanState])],
      warpableObjects: Iterable[WarpableStats],
      objectives: RemainingObjectives, currentTurn: TurnStartedEvt,
      extractionSpeeds: Set[ExtractionSpeed]
    ) extends NetClientOut
    case class Events(events: Vector[FinalEvent]) extends NetClientOut
    case class Error(error: String) extends NetClientOut
  }

  private[this] def initMsg(human: Human, gaGame: GameActorGame)
  (implicit log: LoggingAdapter): String \/ NetClientOut.Init = {
    val game = gaGame.game
    val visibleGame = game.visibleBy(human)
    val states = visibleGame.states
    val resourceMap = visibleGame.world.resourcesMap
    def stateFor(p: Player): String \/ HumanState = for {
      gameState <- states.get(p).
        toRightDisjunction(s"can't get game state for $p in $states")
      resources <- resourceMap.get(p).
        toRightDisjunction(s"can't get game state for $p in $resourceMap")
    } yield HumanState(resources, visibleGame.world.populationFor(p), gameState)

    stateFor(human).map { selfState =>
      NetClientOut.Init(
        game.world.id, visibleGame.world.bounds,
        visibleGame.world.objects ++
          game.world.noLongerVisibleImmovableObjectsFor(human.team),
        visibleGame.world.warpZoneMap.map.keys.map(_._1),
        visibleGame.world.visibilityMap.map.keys.map(_._1),
        human.team, game.world.teams - human.team, selfState,
          (game.world.players - human).map { player =>
          player -> (
            if (player.isFriendOf(human)) stateFor(player).toOption
            else None
          )
        },
        selfState.gameState.canWarp,
        game.remainingObjectives(human.team),
        TurnStartedEvt(gaGame.currentPlayer, gaGame.currentTurnTimeframe),
        ExtractionSpeed.values
      )
    }
  }

  private def events(
    human: Human, ref: NetClientOutRef, events: Events
  )(implicit log: LoggingAdapter): Unit = {
    log.debug("### Dispatching events for {} ###", human)
    log.debug("Events ({}):", events.size)
    val viewedEvents = events.flatMap { event =>
      log.debug("* {}", event)
      val viewed = event.asViewedBy(human)
      if (log.isDebugEnabled) viewed.foreach(log.debug("*** {}", _))
      viewed
    }

    ref ! NetClientOut.Events(viewedEvents)
  }

  case class StartingHuman(
    human: Human, resources: Resources,
    replyJoined: NetClientJoinedRef, client: NetClientOutRef
  ) {
    def game = Game.StartingPlayer(human, resources)
  }

  def behavior(
    worldMaterializer: WorldMaterializer,
    turnTimerSettings: Option[TurnTimers.Settings],
    aiTeam: Team, starting: Set[GameActor.StartingHuman]
  ): Behavior[In] = ContextAware[Message] { ctx =>
    implicit val log = ctx.createLogging()

    def scheduleCheckTurnTime() =
      ctx.schedule(1.second, ctx.self, Internal.CheckTurnTime)

    log.debug(
      "initializing game actor: starting={} turnTimer={}, aiTeam={}",
      starting, turnTimerSettings, aiTeam
    )

    var clients = starting.map(data => data.human -> data.client).toMap

    var game: GameActorGame = {
      val humanTeams = starting.map(_.human.team)
      val world = worldMaterializer.materialize(humanTeams).right_!
      log.debug("World initialized to {}", world)
      val objectives = Map(
        aiTeam -> Objectives(
          destroyAllCriticalObjects = Some(Objective.DestroyAllCriticalObjects)
        )
      ) ++ humanTeams.map { _ -> Objectives(
  //      gatherResources = Some(Objective.GatherResources(world, Resources(200), Percentage(0.15))),
  //      collectVps = Some(Objective.CollectVPs(VPS(10))),
        destroyAllCriticalObjects = Some(Objective.DestroyAllCriticalObjects)
      ) }.toMap
      log.debug("Objectives initialized to {}", objectives)
      SemiRealtimeGame(
        world, starting.map(_.game), objectives,
        turnTimerSettings.map(WithCurrentTime(_, DateTime.now))
      ).fold(
        err => throw new IllegalStateException(s"Cannot initialize game: $err"),
        evented => {
          log.debug("Turn based game initialized to {}", evented)
          starting.foreach { data =>
            val init = initMsg(data.human, evented.value).right_!

            data.replyJoined ! NetClient.LoggedInState.GameJoined(data.human, ctx.self)
            // We need to init the game to starting state.
            data.client ! init
            events(data.human, data.client, evented.events)
          }
          starting.foreach { data =>
            events(data.human, data.client, evented.events)
          }
          evented.value
        }
      )
    }

    def checkedTurnTimes = game.checkTurnTimes(DateTime.now)

    def update(
      requester: NetClientOutRef, f: GameActorGame => GameActorGame.Result
    ): Behavior[Message] = {
      log.debug("Updating game by a request from {}", requester)

      val afterTimeCheck = checkedTurnTimes
      afterTimeCheck.value.fold(
        _ => postGameChange(afterTimeCheck),
        tbg => f(tbg).map(evt => afterTimeCheck.events ++: evt).fold(
          err => {
            log.error(err)
            requester ! NetClientOut.Error(err)
            Same
          },
          postGameChange
        )
      )
    }

    def postGameChange(
      evented: Evented[Winner \/ GameActorGame]
    ): Behavior[Message] = {
      dispatchEvents(evented.events)
      evented.value.fold(
        winner => {
          log.info("Game is finished, won by {}", winner)
          Stopped
        },
        g => {
          game = g
          Same
        }
      )
    }

    def dispatchEvents(events: Events): Unit = {
      if (events.nonEmpty) clients.foreach { case (human, ref) =>
        GameActor.events(human, ref, events)
      }
    }

    var turnTimerChecker = scheduleCheckTurnTime()

    Full {
      case Sig(_, PostStop) =>
        turnTimerChecker.cancel()
        Same
        
      case Msg(_, msg) => msg match {
        case Internal.CheckTurnTime =>
          postGameChange(checkedTurnTimes)
          turnTimerChecker = scheduleCheckTurnTime()
          Same

        case In.Join(ClientData(human, replyTo), joinedRef) =>
          joinedRef ! NetClient.LoggedInState.GameJoined(human, ctx.self)
          def doInit(gaGame: GameActorGame): Unit = {
            replyTo ! initMsg(human, gaGame).right_!
            clients += human -> replyTo
          }
  
          if (game.isJoined(human)) {
            log.info("Rejoining {} to {}", human, ctx.self)
            doInit(game)
          }
          else {
            log.error("Unknown human trying to join the game: {}", human)
          }
          Same

        case In.Warp(clientData, position, warpable) =>
          update(clientData.replyTo, _.warp(clientData.human, position, warpable, DateTime.now))
        case In.Move(clientData, id, path) =>
          update(clientData.replyTo, _.move(clientData.human, id, path, DateTime.now))
        case In.Attack(clientData, id, target) =>
          update(clientData.replyTo, _.attack(clientData.human, id, target, DateTime.now))
        case In.MoveAttack(move, target) =>
          update(
            move.clientData.replyTo,
            _.moveAttack(move.clientData.human, move.id, move.path, target, DateTime.now)
          )
        case In.Special(clientData, id) =>
          update(clientData.replyTo, _.special(clientData.human, id, DateTime.now))
        case In.ToggleWaitingForRoundEnd(clientData) =>
          update(clientData.replyTo, _.toggleWaitingForRoundEnd(clientData.human, DateTime.now))
        case In.Concede(clientData) =>
          update(clientData.replyTo, _.concede(clientData.human, DateTime.now))
      }
    }
  }.narrow
}

object GameActorGame {
  type Result = Game.ResultT[Winner \/ GameActorGame]
}
trait GameActorGame {
  import GameActorGame._

  def warp(human: Human, position: Vect2, warpable: WarpableCompanion.Some, now: DateTime)
  (implicit log: LoggingAdapter): Result

  def move(human: Human, id: WObject.Id, path: NonEmptyVector[Vect2], now: DateTime)
  (implicit log: LoggingAdapter): Result

  def special(human: Human, id: WObject.Id, now: DateTime)(implicit log: LoggingAdapter): Result

  def attack(human: Human, id: WObject.Id, target: Vect2 \/ WObject.Id, now: DateTime)
  (implicit log: LoggingAdapter): Result

  def moveAttack(
    human: Human, id: Id, path: NonEmptyVector[Vect2], target: Vect2 \/ WObject.Id,
    now: DateTime
  )(implicit log: LoggingAdapter): Result

  def toggleWaitingForRoundEnd(human: Human, now: DateTime)(implicit log: LoggingAdapter): Result
  def concede(human: Human, now: DateTime)(implicit log: LoggingAdapter): Result

  def game: Game
  def isJoined(human: Human)(implicit log: LoggingAdapter): Boolean
  def currentPlayer: Player
  def currentTurnTimeframe: Option[Timeframe]
  def currentTurnStartedEvt = TurnStartedEvt(currentPlayer, currentTurnTimeframe)

  def checkTurnTimes(time: DateTime)(implicit log: LoggingAdapter)
  : Evented[Winner \/ GameActorGame]
}

trait GameActorGameStarter[GAGame <: GameActorGame] {
  type StartedGame = String \/ Evented[GAGame]

  def apply(
    world: World, starting: Set[Game.StartingPlayer],
    objectives: Game.ObjectivesMap,
    turnTimerSettings: Option[WithCurrentTime[TurnTimers.Settings]]
  )(implicit log: LoggingAdapter): StartedGame = {
    val game = Game(world, starting, objectives)
    game.flatMap(apply(_, turnTimerSettings))
  }

  def apply(game: Game, turnTimerSettings: Option[WithCurrentTime[TurnTimers.Settings]])
  (implicit log: LoggingAdapter): StartedGame = {
    val turnTimers = turnTimerSettings.map(_.map(TurnTimers(game.world.humans, _)))
    startNewGame(game, turnTimers)
  }

  protected[this] def startNewGame(
    game: Game, turnTimers: Option[WithCurrentTime[TurnTimers]]
  )(implicit log: LoggingAdapter): String \/ Evented[GAGame]
}