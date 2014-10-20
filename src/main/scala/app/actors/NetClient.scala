package app.actors

import java.util.UUID

import akka.actor._
import akka.event.LoggingReceive
import app.actors.game.GameActor
import app.models._
import app.models.game.Human

object NetClient {
  type GameInMsg = Human => GameActor.In

  object Management {
    sealed trait In
    object In {
      case class Login(name: String, password: String) extends In
      case object JoinGame extends In
    }

    sealed trait Out
    object Out {
      sealed trait LoginResponse extends Out
      case object InvalidCredentials extends LoginResponse
      case class LoggedIn(id: UUID) extends LoginResponse

      case class GameJoined(human: Human) extends Out
    }
  }

  object Msgs {
    sealed trait FromClient
    object FromClient {
      case class Game(msg: GameInMsg) extends FromClient
      case class Management(msg: NetClient.Management.In) extends FromClient
    }

    sealed trait FromServer
    object FromServer {
      case class Game(msg: GameActor.ClientOut) extends FromServer
      case class Management(msg: NetClient.Management.Out) extends FromServer
    }
  }
}

class NetClient(
  msgHandler: ActorRef, gamesManager: ActorRef
) extends Actor with ActorLogging {
  import app.actors.NetClient.Management.In._
  import app.actors.NetClient.Management.Out._
  import app.actors.NetClient.Msgs._
  import app.actors.NetClient._

  implicit class ServerMsgExts(msg: FromServer) {
    def out(): Unit = msgHandler ! msg
  }
  implicit class ManagementMsgExts(msg: Management.Out) {
    def out(): Unit = FromServer.Management(msg).out()
  }
  implicit class GameMsgExts(msg: GameActor.ClientOut) {
    def out(): Unit = FromServer.Game(msg).out()
  }

  context.watch(msgHandler)

  override def receive = notLoggedIn

  private[this] def common: Receive = new Receive {
    override def isDefinedAt(x: Any) = false
    override def apply(v1: Any) = ???
  }

  private[this] def notLoggedIn: Receive = LoggingReceive(({
    case FromClient.Management(Login(name, password)) =>
      val user = User(UUID.nameUUIDFromBytes(name.getBytes), name) // Fake login here
      LoggedIn(user.id).out()
      context.become(loggedIn(user))
  }: Receive) orElse common)

  private[this] def loggedIn(user: User): Receive = LoggingReceive(({
    case FromClient.Management(JoinGame) =>
      gamesManager ! GameActor.In.Join(user)

    case GameActor.Out.Joined(human, game) =>
      GameJoined(human).out()
      context.become(inGame(human, game))
  }: Receive) orElse common)

  private[this] def inGame(human: Human, game: ActorRef): Receive = LoggingReceive(({
    case FromClient.Game(msgFn) =>
      val msg = msgFn(human)
      game ! msg
    case msg: GameActor.ClientOut =>
      msg.out()
  }: Receive) orElse common)
}


