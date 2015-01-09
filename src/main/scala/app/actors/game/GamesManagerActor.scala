package app.actors.game

import akka.actor.SupervisorStrategy.Stop
import akka.actor.{Terminated, OneForOneStrategy, Actor, ActorRef}
import akka.event.LoggingReceive
import app.models.User
import implicits._

class GamesManagerActor extends Actor {
  private[this] var user2game = Map.empty[User, ActorRef]
  private[this] var game2user = Map.empty[ActorRef, User]

  override def supervisorStrategy = OneForOneStrategy() {
    case _ => Stop
  }

  override def receive = LoggingReceive {
    case msg @ GameActor.In.Join(user) =>
      user2game.get(user).fold2(createGame(user, sender()), _.tell(msg, sender()))
    case Terminated(ref) =>
      game2user.get(ref).foreach { user =>
        game2user -= ref
        user2game -= user
      }
  }

  private[this] def createGame(user: User, client: ActorRef): ActorRef = {
    val game = context.actorOf(GameActor.props(user, client))
    context.watch(game)
    user2game += user -> game
    game2user += game -> user
    game
  }
}
