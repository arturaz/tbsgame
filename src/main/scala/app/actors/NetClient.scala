package app.actors

import java.nio.ByteOrder
import java.util.UUID

import akka.actor._
import akka.event.LoggingReceive
import akka.io.Tcp.Register
import app.actors.game.GameActor
import app.models._
import app.models.game.Human
import utils.actors.{CodedFrameProxy, IntFramedProxy}

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
      case class Game(msg: GameActor.Out) extends FromServer
      case class Management(msg: NetClient.Management.Out) extends FromServer
    }
  }

  /* Returns chain of connection <-> intFramedProxy <-> codedFrameProxy <-> netClient */
  def startConnectionHandler(
    context: ActorContext, connection: ActorRef, gamesManager: ActorRef, name: String
  )(implicit byteOrder: ByteOrder): ActorRef = {
    val intFramedProxy =
      context.actorOf(Props(new IntFramedProxy), s"$name-int-framed-proxy")
    val codedFrameProxy =
      context.actorOf(Props(new CodedFrameProxy(
        ProtobufCoding.Parsing.parse, ProtobufCoding.Serializing.serialize
      )), s"$name-coded-frame-proxy")
    val netClient =
      context.actorOf(Props(new NetClient(
        codedFrameProxy, gamesManager
      )), s"$name-net-client")

    intFramedProxy ! IntFramedProxy.Init(connection, codedFrameProxy)
    codedFrameProxy ! CodedFrameProxy.Init(intFramedProxy, netClient)
    connection ! Register(intFramedProxy)
    netClient
  }
}

class NetClient(
  codedFrameProxy: ActorRef, gamesManager: ActorRef
) extends Actor with ActorLogging {
  import app.actors.NetClient.Management.In._
  import app.actors.NetClient.Management.Out._
  import app.actors.NetClient.Msgs._
  import app.actors.NetClient._

  implicit class ServerMsgExts(msg: FromServer) {
    def out(): Unit = codedFrameProxy ! msg
  }
  implicit class ManagementMsgExts(msg: Management.Out) {
    def out(): Unit = FromServer.Management(msg).out()
  }
  implicit class GameMsgExts(msg: GameActor.Out) {
    def out(): Unit = FromServer.Game(msg).out()
  }

  context.watch(codedFrameProxy)

  override def receive = notLoggedIn

  private[this] def notLoggedIn: Receive = LoggingReceive {
    case FromClient.Management(Login(name, password)) =>
      val user = User(UUID.randomUUID(), name) // Fake login here
      LoggedIn(user.id).out()
      context.become(loggedIn(user))
  }

  private[this] def loggedIn(user: User): Receive = LoggingReceive {
    case FromClient.Management(JoinGame) =>
      gamesManager ! GameActor.In.Join(user)

    case GameActor.Out.Joined(human, game) =>
      GameJoined(human).out()
      context.become(inGame(human, game))
  }

  private[this] def inGame(human: Human, game: ActorRef): Receive = LoggingReceive {
    case FromClient.Game(msgFn) =>
      val msg = msgFn(human)
      game ! msg
    case msg: GameActor.Out =>
      msg.out()
  }
}


