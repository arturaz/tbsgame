package app.actors.game

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.{LoggingAdapter, LoggingReceive}
import app.actors.game.GameActor.Out.Joined
import app.algorithms.Pathfinding.Path
import app.models.game.TurnBasedGame.WithCurrentTime
import app.models.game._
import app.models.game.events._
import app.models.game.world._
import app.models.game.world.buildings._
import app.models.game.world.maps.WorldMaterializer
import app.models.game.world.props.{ExtractionSpeed, AsteroidStats}
import app.models.game.world.units._
import implicits._
import org.joda.time.DateTime
import utils.data.{Timeframe, NonEmptyVector}

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.language.existentials
import scalaz.{\/-, -\/, \/}

object GameActor {
  sealed trait In
  object In {
    case object CheckTurnTime extends In
    case class Join(human: Human) extends In
    case class Leave(human: Human) extends In
    case class Warp(
      human: Human, position: Vect2, warpable: WarpableCompanion.Some
    ) extends In
    /* path does not include objects position and ends in target position */
    case class Move(human: Human, id: WObject.Id, path: NonEmptyVector[Vect2]) extends In
    case class Attack(human: Human, id: WObject.Id, targetId: WObject.Id) extends In
    case class MoveAttack(
      human: Human, id: WObject.Id, path: NonEmptyVector[Vect2], targetId: WObject.Id
    ) extends In
    case class Special(human: Human, id: WObject.Id) extends In
    case class EndTurn(human: Human) extends In
    case class Concede(human: Human) extends In
  }

  sealed trait Out
  sealed trait ClientOut extends Out
  object Out {
    case class Joined(human: Human, game: ActorRef) extends Out
    case class Init(
      id: World.Id, bounds: Bounds, objects: WorldObjs.All,
      warpZonePoints: Iterable[Vect2], visiblePoints: Iterable[Vect2],
      selfTeam: Team, otherTeams: Iterable[Team],
      self: HumanState, others: Iterable[(Player, Option[HumanState])],
      warpableObjects: Iterable[WarpableStats],
      attackMultipliers: Set[(WObjKind, WObjKind)],
      objectives: RemainingObjectives, turnTimeframe: Option[Timeframe],
      extractionSpeeds: Set[ExtractionSpeed]
    ) extends ClientOut
    case class Events(events: Vector[FinalEvent]) extends ClientOut
    case class Error(error: String) extends ClientOut
  }

  private[this] def initMsg(human: Human, tbgame: TurnBasedGame)
  (implicit log: LoggingAdapter): Either[String, Out.Init] = {
    val game = tbgame.game
    val visibleGame = game.visibleBy(human)
    val states = visibleGame.states
    val resourceMap = visibleGame.world.resourcesMap
    def stateFor(p: Player): Either[String, HumanState] = for {
      gameState <- states.get(p).
        toRight(s"can't get game state for $p in $states").right
      resources <- resourceMap.get(p).
        toRight(s"can't get game state for $p in $resourceMap").right
    } yield HumanState(resources, visibleGame.world.populationFor(p), gameState)

    stateFor(human).right.map { selfState =>
      Out.Init(
        game.world.id, visibleGame.world.bounds,
        visibleGame.world.objects ++
          game.world.noLongerVisibleImmovableObjectsFor(human.team),
        visibleGame.world.warpZoneMap.map.keys.map(_._1),
        visibleGame.world.visibilityMap.map.keys.map(_._1),
        human.team, game.world.teams - human.team, selfState,
          (game.world.players - human).map { player =>
          player -> (
            if (player.isFriendOf(human)) stateFor(player).right.toOption
            else None
          )
        },
        selfState.gameState.canWarp,
        for (from <- WObjKind.All; to <- WObjKind.All) yield from -> to,
        game.remainingObjectives(human.team),
        tbgame.turnTimeframeFor(human), ExtractionSpeed.values
      )
    }
  }

  private def init(
    human: Human, ref: ActorRef, tbgame: TurnBasedGame
  )(implicit log: LoggingAdapter): Unit =
    initMsg(human, tbgame).fold(
      err => throw new IllegalStateException(s"cannot init game state: $err"),
      msg => {
        ref ! msg
        events(human, ref, Vector(TurnStartedEvt(tbgame.currentTeam)))
      }
    )

  private def events(
    human: Human, ref: ActorRef, events: Events
  )(implicit log: LoggingAdapter): Unit = {
    log.debug("### Dispatching events for {} ###", human)
    log.debug("Events ({}):", events.size)
    val viewedEvents = events.flatMap { event =>
      log.debug("* {}", event)
      val viewed = event.asViewedBy(human)
      if (log.isDebugEnabled) viewed.foreach(log.debug("*** {}", _))
      viewed
    }

    ref ! Out.Events(viewedEvents)
  }

  private def nextReadyTeam(
    game: TurnBasedGame, currentTime: DateTime
  )(implicit log: LoggingAdapter): Evented[Winner \/ TurnBasedGame] = {
    @tailrec def rec(
      gameOrWinner: Evented[Winner \/ TurnBasedGame]
    ): Evented[Winner \/ TurnBasedGame] = {
      if (gameOrWinner.value.fold(_ => true, ! _.currentTeamFinished)) gameOrWinner
      else rec(gameOrWinner.flatMap {
        case left @ -\/(winner) => Evented(left)
        case \/-(game) => game.nextTeamTurn(currentTime)
      })
    }

    rec(Evented(game.rightZ))
  }

  def props(
    worldMaterializer: WorldMaterializer, turnTimerSettings: Option[TurnTimers.Settings],
    aiTeam: Team, starting: Set[GameActor.StartingHuman]
  ) = Props(new GameActor(worldMaterializer, turnTimerSettings, aiTeam, starting))

  case class StartingHuman(human: Human, resources: Resources, client: ActorRef) {
    def game = Game.StartingPlayer(human, resources)
  }
}

class GameActor private (
  worldMaterializer: WorldMaterializer, turnTimerSettings: Option[TurnTimers.Settings],
  aiTeam: Team, starting: Set[GameActor.StartingHuman]
) extends Actor with ActorLogging {
  import app.actors.game.GameActor._
  import context.dispatcher
  implicit val logging = log

  log.debug(
    "initializing game actor: starting={} turnTimer={}, aiTeam={}",
    starting, turnTimerSettings, aiTeam
  )

  private[this] var clients = starting.map(data => data.human -> data.client).toMap

  private[this] var game = {
    val humanTeams = starting.map(_.human.team)
    val world = worldMaterializer.materialize(humanTeams, aiTeam).right_!
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
    TurnBasedGame(
      world, starting.map(_.game), objectives,
      turnTimerSettings.map(WithCurrentTime(_, DateTime.now))
    ).fold(
      err => throw new IllegalStateException(s"Cannot initialize game: $err"),
      evented => {
        log.debug("Turn based game initialized to {}", evented)
        starting.foreach { data =>
          data.client ! Joined(data.human, self)
          // We need to init the game to starting state.
          init(data.human, data.client, evented.value)
          events(data.human, data.client, evented.events)
        }
        // And then get to the state where next ready team can act
        val turnStartedGame = evented.flatMap(nextReadyTeam(_, DateTime.now))
        starting.foreach { data =>
          events(data.human, data.client, turnStartedGame.events)
        }
        // TODO: what to do if the game is won from the beginning?
        turnStartedGame.value.right_!
      }
    )
  }

  val turnTimerChecker =
    context.system.scheduler.schedule(1.second, 1.second, self, In.CheckTurnTime)

  @throws[Exception](classOf[Exception])
  override def postStop() = {
    super.postStop()
    turnTimerChecker.cancel()
  }
  
  val notLoggedReceive: Receive = {
    case In.CheckTurnTime =>
      postGameChange(checkedTurnTimes)
  }

  val loggedReceive = LoggingReceive {
    case In.Join(human) =>
      val ref = sender()
      ref ! Out.Joined(human, self)
      def doInit(tbg: TurnBasedGame): Unit = {
        init(human, ref, tbg)
        clients += human -> ref
      }

      if (game.isJoined(human)) {
        log.info("Rejoining {} to {}", human, self)
        doInit(game)
      }
      else {
        log.error("Unknown human trying to join the game: {}", human)
        // TODO: allow new joins?
        //        update(
        //          ref, human,
        //          _.join(human, GameActor.StartingResources).right.map { evtTbg =>
        //            doInit(evtTbg.value)
        //            evtTbg
        //          }
        //        )
      }
//    case In.Leave(human) =>
//      if (clients.contains(human)) {
//        update(sender(), human, _.leave(human).right.map { evtTbg =>
//          clients -= human
//          evtTbg
//        })
//      }
//      else {
//        sender ! Out.Error(s"No human $human is joined.")
//      }
    case In.Warp(human, position, warpable) =>
      update(sender(), human, _.warp(human, position, warpable))
    case In.Move(human, id, path) =>
      update(sender(), human, _.move(human, id, path))
    case In.Attack(human, id, target) =>
      update(sender(), human, _.attack(human, id, target))
    case In.MoveAttack(human, id, path, target) =>
      update(sender(), human, _.moveAttack(human, id, path, target))
    case In.Special(human, id) =>
      update(sender(), human, _.special(human, id))
    case In.EndTurn(human) =>
      update(sender(), human, _.endTurn(human, DateTime.now))
    case In.Concede(human) =>
      update(sender(), human, _.concede(human))
  }

  val receive: PartialFunction[Any, Unit] = notLoggedReceive orElse loggedReceive

  private[this] def updatedGame(f: => Evented[TurnBasedGame])
  : Evented[Winner \/ TurnBasedGame] = f.flatMap(nextReadyTeam(_, DateTime.now))

  private[this] def checkedTurnTimes = updatedGame(game.checkTurnTimes(DateTime.now))

  private[this] def update(
    requester: ActorRef, human: Human, f: TurnBasedGame => TurnBasedGame.Result
  ): Unit = {
    log.debug("Updating game by a request from {}", requester)

    val afterTimeCheck = checkedTurnTimes
    afterTimeCheck.value.fold(
      _ => postGameChange(afterTimeCheck),
      tbg => f(tbg).right.map(evt => updatedGame(afterTimeCheck.events ++: evt)).fold(
        err => {
          log.error(err)
          requester ! Out.Error(err)
        },
        postGameChange
      )
    )
  }

  private[this] def postGameChange(evented: Evented[Winner \/ TurnBasedGame]): Unit = {
    dispatchEvents(evented.events)
    evented.value.fold(
      winner => {
        log.info("Game is finished, won by {}", winner)
        context.stop(self)
      },
      g => game = g
    )
  }

  private[this] def dispatchEvents(events: Events): Unit = {
    if (events.nonEmpty) clients.foreach { case (human, ref) =>
      GameActor.events(human, ref, events)
    }
  }
}
