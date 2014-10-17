package app.actors.game

import akka.actor.{ActorLogging, Props, Actor, ActorRef}
import akka.event.{LoggingAdapter, LoggingReceive}
import app.actors.game.GameActor.Out.Joined
import app.models.game._
import app.models.game.events.{Evented, TurnStartedEvt, Events => GEvents}
import app.models.game.world._
import app.models.User
import infrastructure.Log
import language.existentials
import implicits._

import scala.annotation.tailrec

object GameActor {
  val StartingResources = Resources(25)
  val HumanTeam = Team()
  val AiTeam = Team()

  sealed trait In
  object In {
    case class Join(user: User) extends In
    case class Leave(human: Human) extends In
    case class Warp(
      human: Human, position: Vect2, warpable: WarpableCompanion[_ <: Warpable]
    ) extends In
    case class Move(human: Human, id: WObject.Id, to: Vect2) extends In
    case class Attack(human: Human, id: WObject.Id, targetId: WObject.Id) extends In
    case class Special(human: Human, id: WObject.Id) extends In
    case class ConsumeActions(human: Human) extends In
  }

  sealed trait Out
  sealed trait ClientOut extends Out
  object Out {
    case class Joined(human: Human, game: ActorRef) extends Out
    case class Init(game: Game) extends ClientOut
    case class Events(events: GEvents) extends ClientOut
    case class Error(error: String) extends ClientOut
  }

  private def init(human: Human, ref: ActorRef, game: Game): Unit = {
    ref ! GameActor.Out.Init(game.visibleBy(human))
  }

  private def events(
    human: Human, ref: ActorRef, events: GEvents
  ): Unit = {
    ref ! Out.Events(events.filter(_.visibleBy(human)))
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
    val ai = Bot(AiTeam)
    val world = World.create(HumanTeam, ai, ai)
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
      val human = Human(user.name, HumanTeam, user.id)
      val ref = sender()
      ref ! Out.Joined(human, self)
      update(
        ref,
        _.join(human, GameActor.StartingResources).right.map { evtTbg =>
          init(human, ref, evtTbg.value.game)
          if (evtTbg.value.currentTeam === human.team)
            events(human, ref, Vector(TurnStartedEvt(human.team)))
          clients += human -> ref
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
    case In.Move(human, id, to) =>
      update(sender(), _.move(human, id, to))
    case In.Attack(human, id, target) =>
      update(sender(), _.attack(human, id, target))
    case In.Special(human, id) =>
      update(sender(), _.special(human, id))
    case In.ConsumeActions(human) =>
      update(sender(), _.consumeActions(human))
  }

  private[this] def update(
    requester: ActorRef, f: TurnBasedGame => TurnBasedGame.Result
  ): Unit = {
    f(game).right.map(nextReadyTeam).fold(err => {
      Log.error(err)
      requester ! Out.Error(err)
    }, evented => {
      game = evented.value
      dispatchEvents(evented.events)
    })
  }

  private[this] def dispatchEvents(events: GEvents): Unit = {
    clients.foreach { case (human, ref) =>
      ref ! Out.Events(events.filter(_.visibleBy(human)))
    }
  }
}
