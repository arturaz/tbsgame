package app.actors

import java.util.UUID

import akka.actor._
import akka.event.LoggingReceive
import app.actors.NetClient.Management.{PlainPassword, Credentials}
import app.actors.game.GameActor
import app.models._
import app.models.game.Human
import app.persistence.tables.Tables
import implicits._
import app.persistence.DBDriver._

import scala.util.Try

object NetClient {
  type GameInMsg = Human => GameActor.In

  object Management {
    case class PlainPassword(value: String) extends AnyVal {
      import com.github.t3hnar.bcrypt._

      def encrypted = value.bcrypt
    }
    case class Credentials(name: String, password: PlainPassword)

    sealed trait In
    object In {
      case object AutoRegister extends In
      case class CheckNameAvailability(name: String) extends In
      case class ChangeCredentials(credentials: Credentials) extends In
      case class Login(credentials: Credentials) extends In
      case object JoinGame extends In
    }

    sealed trait Out
    object Out {
      case class AutoRegisterResponse(credentials: Credentials) extends Out
      case class CheckNameAvailabilityResponse(name: String, available: Boolean) extends Out
      case class ChangeCredentialsResponse(success: Boolean) extends Out

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
  msgHandler: ActorRef, gamesManager: ActorRef, db: Database
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

  private[this] def notLoggedIn: Receive = {
    def logIn(user: User): Unit = {
      context.become(loggedIn(user))
      LoggedIn(user.id).out()
    }

    LoggingReceive(({
      case FromClient.Management(AutoRegister) =>
        val password = PlainPassword(UUID.randomUUID().toString)
        val id = UUID.randomUUID()
        val user = User(id, s"autogen-${id.shortStr}")
        val credentials = Credentials(user.name, password)
        db.withSession { implicit session =>
          Tables.users.map(t => (t.user, t.autogenerated, t.password)).
            insert((user, true, password.encrypted))
        }
        AutoRegisterResponse(credentials).out()
        logIn(user)

      case FromClient.Management(Login(credentials)) =>
        val idOptQ = Tables.users.filter { t =>
          t.name === credentials.name && t.password === credentials.password.encrypted
        }.map(_.id)
        val idOpt = db.withSession(idOptQ.firstOption(_))

        idOpt.fold2(
          InvalidCredentials.out(),
          id => logIn(User(id, credentials.name))
        )
    }: Receive) orElse common)
  }

  private[this] def loggedIn(user: User): Receive = LoggingReceive(({
    case FromClient.Management(CheckNameAvailability(name)) =>
      val query = Tables.users.map(_.name).filter(_ === name).exists
      val exists = db.withSession(query.run(_))
      CheckNameAvailabilityResponse(name, ! exists).out()

    case FromClient.Management(ChangeCredentials(credentials)) =>
      val query = Tables.users.
        filter(t => t.id === user.id && t.autogenerated === true).
        map(t => (t.name, t.password, t.autogenerated))
      val success = Try {
        db.withSession(query.update((
          credentials.name, credentials.password.encrypted, false
        ))(_))
      }.isSuccess
      ChangeCredentialsResponse(success).out()

    case FromClient.Management(JoinGame) =>
      gamesManager ! GameActor.In.Join(user)

    case GameActor.Out.Joined(human, game) =>
      GameJoined(human).out()
      context.become(inGame(user, human, game))
  }: Receive) orElse common)

  private[this] def inGame(user: User, human: Human, game: ActorRef): Receive = {
    context.watch(game)
    LoggingReceive(({
      case FromClient.Game(msgFn) =>
        val msg = msgFn(human)
        game ! msg
      case msg: GameActor.ClientOut =>
        msg.out()
      case Terminated if sender() == game =>
        log.error("Game was terminated")
        context.become(loggedIn(user))
    }: Receive) orElse common)
  }
}


