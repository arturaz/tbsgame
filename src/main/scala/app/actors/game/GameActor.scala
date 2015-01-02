package app.actors.game

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.{LoggingAdapter, LoggingReceive}
import app.actors.game.GameActor.Out.Joined
import app.algorithms.Pathfinding.Path
import app.models.User
import app.models.game._
import app.models.game.events._
import app.models.game.world._
import app.models.game.world.buildings._
import app.models.game.world.props.Asteroid
import app.models.game.world.units._
import implicits._
import utils.data.NonEmptyVector

import scala.annotation.tailrec
import scala.language.existentials
import scalaz.{\/-, -\/, \/}

object GameActor {
  val StartingResources = Resources(30)
  val HumanTeam = Team()
  val AiTeam = Team()

  sealed trait In
  object In {
    case class Join(user: User) extends In
    case class Leave(human: Human) extends In
    case class Warp(
      human: Human, position: Vect2, warpable: WarpableCompanion.Some
    ) extends In
    case class GetMovement(human: Human, id: WObject.Id) extends In
    /* path does not include objects position and ends in target position */
    case class Move(human: Human, id: WObject.Id, path: NonEmptyVector[Vect2]) extends In
    case class Attack(human: Human, id: WObject.Id, targetId: WObject.Id) extends In
    case class MoveAttack(
      human: Human, id: WObject.Id, path: NonEmptyVector[Vect2], targetId: WObject.Id
    ) extends In
    case class Special(human: Human, id: WObject.Id) extends In
    case class EndTurn(human: Human) extends In
  }

  sealed trait Out
  sealed trait ClientOut extends Out
  object Out {
    case class Joined(human: Human, game: ActorRef) extends Out
    object Init {
      case class Stats(stats: WObjectStats, showInWarpables: Boolean=false)
    }
    case class Init(
      bounds: Bounds, objects: WorldObjs,
      warpZonePoints: Iterable[Vect2], visiblePoints: Iterable[Vect2],
      selfTeam: Team, otherTeams: Iterable[Team],
      self: HumanState, others: Iterable[(Player, Option[HumanState])],
      wObjectStats: Iterable[Init.Stats], attackMultipliers: Set[(WObjKind, WObjKind)],
      objectives: RemainingObjectives
    ) extends ClientOut
    case class Events(events: Vector[FinalEvent]) extends ClientOut
    case class Error(error: String) extends ClientOut
    
    object Movement {
      sealed trait Response
      /* Paths for objects that player can move */
      case class Movable(paths: Vector[Path]) extends Response
      /* Points where object can move to. Send for objects that cannot be moved by
         player. */
      case class Immovable(points: Vector[Vect2]) extends Response
    }
    case class Movement(id: WObject.Id, response: Movement.Response) extends ClientOut
  }

  private[this] def initMsg(human: Human, game: Game): Either[String, Out.Init] = {
    val visibleGame = game.visibleBy(human)
    val states = visibleGame.states
    val resourceMap = visibleGame.world.resourcesMap
    def stateFor(p: Player): Either[String, HumanState] = for {
      gameState <- states.get(human).
        toRight(s"can't get game state for $human in $states").right
      resources <- resourceMap.get(human).
        toRight(s"can't get game state for $human in $resourceMap").right
    } yield HumanState(resources, visibleGame.world.populationLeftFor(p), gameState)

    stateFor(human).right.map { selfState =>
      Out.Init(
        visibleGame.world.bounds, visibleGame.world.objects,
        visibleGame.world.warpZoneMap.map.keys.map(_._1),
        visibleGame.world.visibilityMap.map.keys.map(_._1),
        human.team, game.world.teams - human.team,
        selfState, (game.world.players - human).map { player =>
          player -> stateFor(player).right.toOption
        }, {
          import Out.Init.Stats
          Vector(
            Stats(Asteroid), Stats(WarpGate), Stats(Extractor),
            Stats(WarpLinker, showInWarpables = true),
            Stats(LaserTower, showInWarpables = true),
            Stats(Corvette, showInWarpables = true),
            Stats(RocketFrigate, showInWarpables = true),
            Stats(Gunship, showInWarpables = true),
            Stats(Scout, showInWarpables = true),
            Stats(Spawner), Stats(Wasp), Stats(RayShip), Stats(Fortress)
          )
        },
        for (from <- WObjKind.All; to <- WObjKind.All) yield from -> to,
        game.remainingObjectives(human.team)
      )
    }
  }

  private def init(human: Human, ref: ActorRef, game: Game): Unit =
    initMsg(human, game).fold(
      err => throw new IllegalStateException(s"cannot init game state: $err"),
      ref ! _
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
    game: TurnBasedGame
  )(implicit log: LoggingAdapter): Evented[Winner \/ TurnBasedGame] = {
    @tailrec def rec(
      gameOrWinner: Evented[Winner \/ TurnBasedGame]
    ): Evented[Winner \/ TurnBasedGame] = {
      if (gameOrWinner.value.fold(_ => true, ! _.currentTeamFinished)) gameOrWinner
      else rec(gameOrWinner.flatMap {
        case left @ -\/(winner) => Evented(left)
        case \/-(game) => game.nextTeamTurn
      })
    }

    rec(Evented(game.rightZ))
  }

  def props(startingUser: User, startingHumanRef: ActorRef) = Props(new GameActor(
    Human(startingUser, HumanTeam), startingHumanRef
  ))
}

class GameActor private (
  startingHuman: Human, startingHumanRef: ActorRef
) extends Actor with ActorLogging {
  import app.actors.game.GameActor._
  implicit val logging = log

  log.debug(
    "initializing game actor with starting {}, {}", startingHuman, startingHumanRef
  )

  private[this] var clients = Map(startingHuman -> startingHumanRef)

  private[this] var game = {
    val singleAi = Bot(AiTeam)
    val spawnerAi = Bot(AiTeam)
    val world = World.create(
      HumanTeam, singleAi, spawnerAi,
      spawners = 2, endDistance = TileDistance(35)
    )
    log.debug("World initialized to {}", world)
    val objectives = Map(
      AiTeam -> Objectives(
        destroyAllCriticalObjects = Some(Objective.DestroyAllCriticalObjects)
      ),
      HumanTeam -> Objectives(
        Some(Objective.GatherResources(world, Resources(100), Percentage(0.1))),
        Some(Objective.CollectVPs(VPS(10))),
        Some(Objective.DestroyAllCriticalObjects)
      )
    )
    log.debug("Objectives initialized to {}", objectives)
    TurnBasedGame(
      world, startingHuman, GameActor.StartingResources, objectives
    ).fold(
      err => throw new IllegalStateException(s"Cannot initialize game: $err"),
      evented => {
        log.debug("Turn based game initialized to {}", evented)
        startingHumanRef ! Joined(startingHuman, self)
        // We need to init the game to starting state.
        init(startingHuman, startingHumanRef, evented.value.game)
        events(startingHuman, startingHumanRef, evented.events)
        // And then get to the state where next ready team can act
        val turnStartedGame = evented.flatMap(nextReadyTeam)
        events(startingHuman, startingHumanRef, turnStartedGame.events)
        // TODO: what to do if the game is won from the beggining?
        turnStartedGame.value.right_!
      }
    )
  }

  def receive = LoggingReceive {
    case In.Join(user) =>
      val human = Human(user.name, HumanTeam, Player.Id(user.id))
      val ref = sender()
      ref ! Out.Joined(human, self)
      def doInit(tbg: TurnBasedGame): Unit = {
        init(human, ref, tbg.game)
        if (tbg.currentTeam === human.team)
          events(human, ref, Vector(TurnStartedEvt(human.team)))
        clients += human -> ref
      }

      if (game.isJoined(human)) {
        log.info("Rejoining {} to {}", human, self)
        doInit(game)
      }
      else update(
        ref, human,
        _.join(human, GameActor.StartingResources).right.map { evtTbg =>
          doInit(evtTbg.value)
          evtTbg
        }
      )
    case In.Leave(human) =>
      if (clients.contains(human)) {
        update(sender(), human, _.leave(human).right.map { evtTbg =>
          clients -= human
          evtTbg
        })
      }
      else {
        sender ! Out.Error(s"No human $human is joined.")
      }
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
      update(sender(), human, _.endTurn(human))
    case In.GetMovement(human, id) =>
      game.game.world.find {
        case o: MovableWObject if o.id == id => o
      }.toRight(s"Can't find movable world object with $id").right.map { obj =>
        (obj, game.game.movementFor(obj))
      }.fold(
        err => sender() ! Out.Error(err),
        { case (obj, paths) =>
          val response =
            if (Game.canMove(human, obj)) Out.Movement.Movable(paths)
            else Out.Movement.Immovable(paths.map(_.vects.last))
          sender() ! Out.Movement(id, response)
        }
      )
  }

  private[this] def update(
    requester: ActorRef, human: Human, f: TurnBasedGame => TurnBasedGame.Result
  ): Unit = {
    log.debug("Updating game by a request from {}", requester)

    def objectivesEvt(game: Game) =
      ObjectivesUpdatedEvt(human.team, game.remainingObjectives(human.team))

    f(game).right.map { evented =>
      val firstEvt = objectivesEvt(evented.value.game)
      val newEvented = (evented :+ firstEvt).flatMap(nextReadyTeam)
      // After nextReadyTeam it might be our turn again, so see if the objectives has
      // changed and notify about it if they did
      newEvented.value.fold(_ => newEvented, newGame => {
        val secondEvt = objectivesEvt(newGame.game)
        if (firstEvt == secondEvt) newEvented else newEvented :+ secondEvt
      })
    }.fold(err => {
      log.error(err)
      requester ! Out.Error(err)
    }, evented => {
      dispatchEvents(evented.events)
      evented.value.fold(
        winner => {
          log.info("Game is finished, won by {}", winner)
          context.stop(self)
        },
        g => game = g
      )
    })
  }

  private[this] def dispatchEvents(events: Events): Unit =
    clients.foreach { case (human, ref) => GameActor.events(human, ref, events) }
}
