package app.actors.game

import akka.actor.{Actor, ActorRef}
import app.models.game.TurnBasedGame
import app.models.game.events.{Evented, TurnStartedEvt, Events => GEvents}
import app.models.world.{Vect2, Warpable, WarpableCompanion, World}
import app.models.{Bot, Human, Team}
import infrastructure.Log

import scala.annotation.tailrec

object GameActor {
  val StartingResources = 25

  object In {
    case class Join(human: Human)
    case class Leave(human: Human)
    case class Warp(
      human: Human, position: Vect2, warpable: WarpableCompanion[_ <: Warpable]
    )
    case class Move(human: Human, from: Vect2, to: Vect2)
    case class Attack(human: Human, source: Vect2, target: Vect2)
    case class Special(human: Human, position: Vect2)
    case class ConsumeActions(human: Human)
  }

  object Out {
    case class Init(world: World)
    case class Events(events: GEvents)
    case class Error(error: String)
  }

  private def init(human: Human, ref: ActorRef, world: World): Unit = {
    ref ! GameActor.Out.Init(world.visibleBy(human))
  }

  private def events(
    human: Human, ref: ActorRef, events: GEvents
  ): Unit = {
    ref ! Out.Events(events.filter(_.visibleBy(human)))
  }

  @tailrec private def nextReadyTeam(
    game: Evented[TurnBasedGame]
  ): Evented[TurnBasedGame] = {
    if (game.value.currentTeamFinished) nextReadyTeam(game.flatMap(_.nextTeamTurn))
    else game
  }
}

class GameActor(
  startingHuman: Human, startingHumanRef: ActorRef
) extends Actor {
  import app.actors.game.GameActor._

  private[this] var clients = Map(startingHuman -> startingHumanRef)

  private[this] var game = {
    val playerTeam = Team()
    val ai = Bot(Team())
    val world = World.create(playerTeam, ai, ai)
    TurnBasedGame(world, GameActor.StartingResources).right.map(nextReadyTeam).fold(
      err => throw new IllegalStateException(s"Cannot initialize game: $err"),
      evented => {
        init(startingHuman, startingHumanRef, evented.value.game.world)
        events(startingHuman, startingHumanRef, evented.events)
        evented.value
      }
    )
  }

  def receive = {
    case In.Join(human) =>
      val ref = sender()
      update(
        ref,
        _.join(human, GameActor.StartingResources).right.map { evtTbg =>
          init(human, ref, evtTbg.value.game.world)
          if (evtTbg.value.currentTeam == human.team)
            events(human, ref, Vector(TurnStartedEvt(human.team)))
          clients += human -> ref
          evtTbg
        }
      )
    case In.Leave(human) =>
      update(sender(), _.leave(human).right.map { evtTbg =>
        clients -= human
        evtTbg
      })
    case In.Warp(human, position, warpable) =>
      update(sender(), _.warp(human, position, warpable))
    case In.Move(human, from, to) =>
      update(sender(), _.move(human, from, to))
    case In.Attack(human, source, target) =>
      update(sender(), _.attack(human, source, target))
    case In.Special(human, position) =>
      update(sender(), _.special(human, position))
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
