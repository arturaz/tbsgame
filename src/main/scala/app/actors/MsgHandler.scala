package app.actors

import java.nio.ByteOrder

import akka.io.Tcp
import akka.typed.ScalaDSL._
import akka.typed._
import akka.util.ByteString
import akka.{actor => untyped}
import app.protobuf.parsing.Parsing
import app.protobuf.serializing.Serializing
import implicits.actor._
import utils.network.IntFramedPipeline
import utils.network.IntFramedPipeline.Frame

import scala.language.implicitConversions
import scalaz.Scalaz._
import scalaz._

object MsgHandler {
  type Ref = ActorRef[In]

  sealed trait Message

  sealed trait In extends Message
  object In {
    sealed trait Control extends In
    object Control {
      case object ShutdownInitiated extends Control with NetClientFwd
    }
    case class FromNetClient(msg: NetClient.MsgHandlerOut) extends In
  }

  // Messages forwarded to NetClient
  sealed trait NetClientFwd
  implicit def asNetClientFwd(msg: NetClientFwd): NetClient.MsgHandlerIn.FwdFromMsgHandler =
    NetClient.MsgHandlerIn.FwdFromMsgHandler(msg)

  private[this] sealed trait Internal extends Message
  private[this] object Internal {
    case class Tcp(msg: akka.io.Tcp.Event) extends Internal
    case object Ack extends akka.io.Tcp.Event with Internal
  }

  def spawn(
    name: String, ctx: ActorContext[_],
    connection: untyped.ActorRef,
    netClientBehavior: Ref => Behavior[NetClient.MsgHandlerIn],
    maxToClientBufferSize: Int = 1024 * 1024
  )(implicit byteOrder: ByteOrder) = {
    lazy val tcpAdapter: ActorRef[Tcp.Event] = ctx.spawn(
      Props(ContextAware[Any] { tcpCtx =>
        tcpCtx.watch(main)
        Full {
          case Msg(_, msg: Message) =>
            main ! msg
            Same
          case Msg(_, msg: Tcp.Event) =>
            main ! Internal.Tcp(msg)
            Same
          case Sig(_, Terminated(`main`)) =>
            Stopped
        }
      }),
      s"$name-tcp-adapter"
    )
    lazy val main: ActorRef[Message] = {
      val bridge = TypedUntypedActorBridge(connection, tcpAdapter.asUntyped)
      ctx.spawn(
        Props(behavior(bridge, netClientBehavior, maxToClientBufferSize)),
        name
      )
    }
    (main: Ref, tcpAdapter)
  }

  private[this] def behavior(
    connection: TypedUntypedActorBridge,
    netClientBehavior: Ref => Behavior[NetClient.MsgHandlerIn],
    maxToClientBufferSize: Int
  )(implicit byteOrder: ByteOrder): Behavior[Message] = ContextAware { ctx =>
    implicit val log = ctx.createLogging()

    val pipeline = new MsgHandlerPipeline
    val lowWatermark = maxToClientBufferSize / 4
    val highWatermark = maxToClientBufferSize * 3 / 4

    var storage = Vector.empty[ByteString]
    var stored = 0
    var closing = false
    var suspended = false

    val netClient = ctx.spawn(Props(netClientBehavior(ctx.self)), "net-client")
    ctx.watch(netClient)
    ctx.watch(connection.raw)

    val common = Partial[Message] {
      case Internal.Tcp(Tcp.Received(data)) =>
        pipeline.unserialize(data).foreach {
          case -\/(err) => log.error(err)
          case \/-(msg) => netClient ! msg
        }
        Same

      case msg: In.Control.ShutdownInitiated.type =>
        netClient ! msg
        Same
    }

    lazy val buffering = Partial[Message] {
      case In.FromNetClient(msg) =>
        buffer(pipeline.serialize(msg)).getOrElse(Same)

      case Internal.Ack =>
        acknowledge()

      case Internal.Tcp(msg: Tcp.ConnectionClosed) =>
        log.info(s"closing = true by {}.", msg)
        closing = true
        Same
    }

    lazy val normal = Partial[Message] {
      case In.FromNetClient(msg) =>
        val data = pipeline.serialize(msg)

        buffer(data).getOrElse {
          connection ! Tcp.Write(data, Internal.Ack)
          buffering
        }

      case Internal.Tcp(msg: Tcp.ConnectionClosed) =>
        log.info(s"Connection closed by {}.", msg)
        Stopped
    }

    def buffer(data: ByteString): Option[Behavior[Message]] = {
      storage :+= data
      stored += data.size

      if (stored > maxToClientBufferSize) {
        log.warning(s"drop connection to [{}] (buffer overrun)", connection)
        Some(Stopped)
      }
      else if (stored > highWatermark) {
        log.debug(s"suspending reading")
        connection ! Tcp.SuspendReading
        suspended = true
        None
      }
      else None
    }

    def acknowledge(): Behavior[Message] = {
      require(storage.nonEmpty, "storage was empty")

      val size = storage.head.size
      stored -= size

      storage = storage.tail

      if (suspended && stored < lowWatermark) {
        log.debug("resuming reading")
        connection ! Tcp.ResumeReading
        suspended = false
      }

      if (storage.isEmpty) {
        if (closing) Stopped else normal
      }
      else {
        connection ! Tcp.Write(storage.head, Internal.Ack)
        Same
      }
    }

    Or(common, normal)
  }
}

class MsgHandlerPipeline(implicit byteOrder: ByteOrder) {
  private[this] val intFramed = new IntFramedPipeline()

  def unserialize(data: ByteString) = intFramed.fromFramedData(data).map { frame =>
    Parsing.parse(frame.data).leftMap(err => s"Cannot decode $frame into message: $err")
  }

  def serialize(data: NetClient.MsgHandlerOut) =
    data |> Serializing.serialize |> Frame |> intFramed.withFrameSize
}