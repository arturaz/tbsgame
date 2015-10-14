package app.actors

import java.net.InetSocketAddress
import java.nio.ByteOrder

import akka.actor._
import akka.io.{IO, Tcp}
import app.persistence.DBDriver
import implicits._
import launch.RTConfig
import spire.math.UInt
import scala.concurrent.duration._
import scalaz._, Scalaz._, implicits._

class Server(
  rtConfig: RTConfig, gamesManager: ActorRef, db: DBDriver.Database
)(implicit byteOrder: ByteOrder) extends Actor with ActorLogging {
  import context.system, context.dispatcher

  def port = rtConfig.port

  val manager = IO(Tcp)
  manager ! Tcp.Bind(self, new InetSocketAddress(port.signed))

  /* Actor that is handling our bound socket. */
  private[this] var socketRef = Option.empty[ActorRef]

  def receive = {
    case Tcp.Bound(localAddress) =>
      socketRef = Some(sender())
      log.info("Server bound to {}", localAddress)

    case msg: Tcp.Unbind.type =>
      socketRef.fold2(
        log.error("Can't unbind, socket not bound to {}", port),
        ref => {
          log.debug("Received a request to unbind, forwarding to {}", ref)
          ref ! msg
        }
      )

    case Tcp.Unbound =>
      socketRef = None
      log.info("Socket to port {} unbound, initiating shutdown.", port)
      context.children.foreach(_ ! Server.ShutdownInitiated)
      gamesManager ! Server.ShutdownInitiated

    case Tcp.CommandFailed(b: Tcp.Bind) =>
      log.error(s"Cannot bind to ${b.localAddress}!")
      context.stop(self)

    case Tcp.Connected(remote, local) =>
      log.info(s"Client connected from $remote.")
      val connection = sender()
      val msgHandler = context.actorOf(Props(new MsgHandler(
        connection,
        handlerRef => Props(new NetClient(handlerRef, gamesManager, self, rtConfig.controlKey, db))
      )), s"${remote.getHostString}-${remote.getPort}")
      connection ! Tcp.Register(msgHandler, keepOpenOnPeerClosed = true)

    case Server.In.ReportClientCount =>
      sender ! Server.Out.ReportClientCount(UInt(context.children.size))
  }

  @throws[Exception](classOf[Exception])
  override def postStop(): Unit = {
    log.info("Shutting down actor system because server has stopped.")
    system.shutdown()
  }
}

object Server {
  sealed trait In
  object In {
    case object ReportClientCount extends In
  }

  sealed trait Out
  object Out {
    case class ReportClientCount(clients: UInt) extends In
  }

  case object ShutdownInitiated
}