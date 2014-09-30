package app.actors.game

import akka.actor.{Actor, ActorRef}
import app.models.game.TurnBasedGame
import app.models.world.World
import app.models.{Bot, Player, Team}

object GameActor {
  val StartingResources = 25
  
  case class Init(world: World)
}

class GameActor(
  startingPlayer: Player, startingPlayerRef: ActorRef
) extends Actor {
  private[this] val playerTeam = Team()
  private[this] val ai = Bot(Team())

  private[this] var game = {
    val world = World.create(playerTeam, () => ai, () => ai)
    TurnBasedGame(world, GameActor.StartingResources)
  }

  def receive = {
    case _ => ()
  }

  private[this] def init(player: Player, ref: ActorRef): Unit = {
//    ref ! GameActor.Init(game.world.visibleBy(player))
  }
}
