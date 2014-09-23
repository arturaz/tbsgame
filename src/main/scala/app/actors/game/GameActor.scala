package app.actors.game

import akka.actor.{ActorRef, Actor}
import app.models.{Team, Player}
import app.models.game.{PlayerState, Game}
import app.models.world.{GivingActions, World}

object GameActor {
  val StartingResources = 25
  
  case class Init(world: World)
}

class GameActor(
  startingPlayer: Player, startingPlayerRef: ActorRef
) extends Actor {
  private[this] val playerTeam = Team()
  private[this] val ai = Player("Worg", Team())

  private[this] var game = {
    val world = World.create(playerTeam, () => ai, () => ai)
    val actions = PlayerState.Actions(world.objects.collect {
      case obj: GivingActions if obj.owner == startingPlayer =>
        obj.companion.actionsGiven
    }.sum)
    Game(
      world,
      Map(startingPlayer -> PlayerState(GameActor.StartingResources, actions))
    )
  }

  def receive = {
    case _ => ()
  }

  private[this] def init(player: Player, ref: ActorRef): Unit = {
//    ref ! GameActor.Init(game.world.visibleBy(player))
  }
}
