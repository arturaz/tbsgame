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
import app.models.game.world.units.{RayShip, Scout, Wasp, Corvette}
import implicits._
import utils.data.NonEmptyVector

import scala.annotation.tailrec
import scala.language.existentials

object GameActor {
  val StartingResources = Resources(25)
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
    case class Move(human: Human, id: WObject.Id, path: Vector[Vect2]) extends In
    case class Attack(human: Human, id: WObject.Id, targetId: WObject.Id) extends In
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
      bounds: Bounds, objects: Iterable[WObject],
      warpZonePoints: Iterable[Vect2], visiblePoints: Iterable[Vect2],
      selfTeam: Team, otherTeams: Iterable[Team],
      self: HumanState, others: Iterable[(Player, Option[HumanState])],
      wObjectStats: Iterable[Init.Stats]
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
    } yield HumanState(resources, gameState)

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
            Stats(Scout, showInWarpables = true),
            Stats(Spawner), Stats(Wasp), Stats(RayShip)
          )
        }
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

  @tailrec private def nextReadyTeam(
    game: Evented[TurnBasedGame]
  )(implicit log: LoggingAdapter): Evented[TurnBasedGame] = {
    if (game.value.currentTeamFinished) nextReadyTeam(game.flatMap(_.nextTeamTurn))
    else game
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
      spawners = 1, blobRichness = 15 to 30, asteroidResources = 8 to 20,
      endDistance = TileDistance(25), waspsAtMaxDistance = 2
    )
    log.debug("World initialized to {}", world)
    TurnBasedGame(
      world, startingHuman, GameActor.StartingResources
    ).right.map(nextReadyTeam).fold(
      err => throw new IllegalStateException(s"Cannot initialize game: $err"),
      evented => {
        log.debug("Turn based game initialized to {}", evented)
        startingHumanRef ! Joined(startingHuman, self)
        init(startingHuman, startingHumanRef, evented.value.game)
        events(startingHuman, startingHumanRef, evented.events)
        evented.value
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
        ref,
        _.join(human, GameActor.StartingResources).right.map { evtTbg =>
          doInit(evtTbg.value)
          evtTbg
        }
      )
    case In.Leave(human) =>
      if (clients.contains(human)) {
        update(sender(), _.leave(human).right.map { evtTbg =>
          clients -= human
          evtTbg
        })
      }
      else {
        sender ! Out.Error(s"No human $human is joined.")
      }
    case In.Warp(human, position, warpable) =>
      update(sender(), _.warp(human, position, warpable))
    case In.Move(human, id, path) =>
      update(sender(), tbg => {
        NonEmptyVector.create(path).toRight(s"movement vector is empty!").right.flatMap(
          tbg.move(human, id, _)
        )
      })
    case In.Attack(human, id, target) =>
      update(sender(), _.attack(human, id, target))
    case In.Special(human, id) =>
      update(sender(), _.special(human, id))
    case In.EndTurn(human) =>
      update(sender(), _.endTurn(human))
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
    requester: ActorRef, f: TurnBasedGame => TurnBasedGame.Result
  ): Unit = {
    log.debug("Updating game by a request from {}", requester)
    f(game).right.map(nextReadyTeam).fold(err => {
      log.error(err)
      requester ! Out.Error(err)
    }, evented => {
      game = evented.value
      dispatchEvents(evented.events)
    })
  }

  private[this] def dispatchEvents(events: Events): Unit =
    clients.foreach { case (human, ref) => GameActor.events(human, ref, events) }
}
