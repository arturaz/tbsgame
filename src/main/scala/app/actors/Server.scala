package app.actors

import java.net.InetSocketAddress
import java.nio.ByteOrder

import akka.io.{IO, Tcp}
import akka.typed.ScalaDSL._
import akka.typed._
import akka.{actor => untyped}
import app.actors.game.GamesManagerActor
import app.persistence.DBDriver
import implicits._
import launch.RTConfig
import spire.math.UInt

object Server {
  type Ref = ActorRef[In]

  private[this] sealed trait Message

  sealed trait In extends Message
  object In {
    case class ReportClientCount(replyTo: ActorRef[Out.ReportClientCount])
      extends In
    case object Unbind extends In
  }

  private[this] sealed trait Internal extends Message
  private[this] object Internal {
    case class Tcp(
      msg: akka.io.Tcp.Message, sender: untyped.ActorRef
    ) extends Internal
    case class MsgHandlerTerminated(ref: MsgHandler.Ref) extends Internal
  }

  sealed trait Out
  object Out {
    case class ReportClientCount(clients: UInt) extends In
  }

  def behavior(
    rtConfig: RTConfig, gamesManager: GamesManagerActor.Ref,
    db: DBDriver.Database
  )(implicit byteOrder: ByteOrder): Behavior[In] = ContextAware[Message] { ctx =>
    def port = rtConfig.port

    val log = ctx.createLogging()
    val tcpHandler = ctx.spawnAdapterUTRef[Tcp.Message](Internal.Tcp)

    val manager = IO(Tcp)(ctx.system.asUntyped)
    manager ! Tcp.Bind(tcpHandler.asUntyped, new InetSocketAddress(port.signed))

    /* Actor that is handling our bound socket. */
    var socketRef = Option.empty[untyped.ActorRef]
    var msgHandlers = Set.empty[MsgHandler.Ref]

    Full {
      case Msg(_, Internal.Tcp(Tcp.Bound(localAddress), sender)) =>
        socketRef = Some(sender)
        log.info("Server bound to {}", localAddress)
        Same

      case Msg(_, In.Unbind) =>
        socketRef.fold2(
          log.error("Can't unbind, socket not bound to {}", port),
          ref => {
            log.debug("Received a request to unbind, forwarding to {}", ref)
            ref ! Tcp.Unbind
          }
        )
        Same

      case Msg(_, Internal.Tcp(Tcp.Unbound, _)) =>
        socketRef = None
        log.info("Socket to port {} unbound, initiating shutdown.", port)
        msgHandlers.foreach(_ ! MsgHandler.In.Control.ShutdownInitiated)
        gamesManager ! GamesManagerActor.In.ShutdownInitiated
        Same

      case Msg(_, Internal.Tcp(Tcp.CommandFailed(b: Tcp.Bind), _)) =>
        log.error(s"Cannot bind to ${b.localAddress}!")
        Stopped

      case Msg(_, Internal.Tcp(Tcp.Connected(remote, local), connection)) =>
        log.info(s"Client connected from $remote.")
        val (msgHandler, tcpAdapter) = MsgHandler.spawn(
          s"${remote.getHostString}-${remote.getPort}", ctx, connection,
          handlerRef => NetClient.behavior(
            handlerRef, gamesManager, ctx.self, rtConfig.controlKey, db
          ).narrow
        )
        msgHandlers += msgHandler
        ctx.watchWith(msgHandler, Internal.MsgHandlerTerminated(msgHandler))
        connection ! Tcp.Register(tcpAdapter.asUntyped, keepOpenOnPeerClosed = true)
        Same

      case Msg(_, In.ReportClientCount(replyTo)) =>
        replyTo ! Server.Out.ReportClientCount(UInt(ctx.children.size))
        Same

      case Msg(_, Internal.MsgHandlerTerminated(ref)) =>
        msgHandlers -= ref
        Same

      case Sig(_, PostStop) =>
        log.info("Shutting down actor system because server has stopped.")
        ctx.system.terminate()
        Stopped
    }
  }.narrow
}