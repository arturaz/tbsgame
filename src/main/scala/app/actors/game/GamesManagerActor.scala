package app.actors.game

import akka.actor.{ActorRef, Actor}
import app.models.User
import implicits._

class GamesManagerActor extends Actor {
  var game = Option.empty[ActorRef]

  override def receive = {
    case msg @ GameActor.In.Join(user) =>
      game.fold2(createGame(user, sender()), _.tell(msg, sender()))
  }

  private[this] def createGame(user: User, client: ActorRef): ActorRef = {
    val gameRef = context.actorOf(GameActor.props(user, client))
    game = Some(gameRef)
    gameRef
  }
}
