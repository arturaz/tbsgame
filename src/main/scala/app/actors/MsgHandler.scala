package app.actors

import java.nio.ByteOrder

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.{LoggingAdapter, LoggingReceive}
import akka.io.Tcp._
import akka.util.ByteString
import app.actors.NetClient.Msgs.{FromControlClient, FromClient, FromServer}
import app.actors.NetClient.Control
import app.protobuf.parsing.Parsing
import app.protobuf.serializing.Serializing
import utils.network.IntFramedPipeline.Frame
import utils.network.IntFramedPipeline
import implicits._
import scalaz._, Scalaz._

/**
 * Created by arturas on 2014-10-15.
 */
object MsgHandler {
  private case object Ack extends Event

  // We need this because we can't pattern match in Receive on \/
  sealed trait Server2Client {
    def toEither = this match {
      case Server2Client.GameMsg(msg) => msg.left
      case Server2Client.ControlMsg(msg) => msg.right
    }
  }
  object Server2Client {
    case class GameMsg(msg: NetClient.Msgs.FromServer) extends Server2Client
    case class ControlMsg(msg: NetClient.Control.Out) extends Server2Client
  }

  sealed trait Client2Server {
    def message: Serializable
  }
  object Client2Server {
    case class GameMsg(message: NetClient.Msgs.FromClient) extends Client2Server
    case class ControlMsg(message: NetClient.Msgs.FromControlClient) extends Client2Server
    // Background searching for opponent heartbeat
    case class BackgroundSFOHeartbeat(token: String) extends Client2Server {
      override def message = this
    }
  }
}

class MsgHandler(
  connection: ActorRef, netClientProps: ActorRef => Props,
  maxToClientBufferSize: Int = 1024 * 1024
)(implicit byteOrder: ByteOrder)
extends Actor with ActorLogging {
  import MsgHandler._

  context.watch(connection)

  private[this] val netClient =
    context.actorOf(netClientProps(self), "net-client")
  context.watch(netClient)

  private[this] implicit val logger = log
  private[this] val pipeline = new MsgHandlerPipeline
  private[this] val lowWatermark = maxToClientBufferSize / 4
  private[this] val highWatermark = maxToClientBufferSize * 3 / 4

  private[this] var storage = Vector.empty[ByteString]
  private[this] var stored = 0
  private[this] var closing = false
  private[this] var suspended = false

  private[this] val fromClient: Receive = {
    case Received(data) => pipeline.unserialize(data).foreach {
      case -\/(err) => log.error(err)
      case \/-(clientOrControlMsg) => netClient ! clientOrControlMsg.message
    }
  }

  private[this] val buffering = {
    LoggingReceive(fromClient orElse {
      case msg: MsgHandler.Server2Client =>
        buffer(pipeline.serialize(msg))

      case Ack =>
        acknowledge()

      case msg: ConnectionClosed =>
        log.info(s"closing = true by {}.", msg)
        closing = true
    })
  }

  override val receive = LoggingReceive(fromClient orElse {
    case msg: Server2Client =>
      val data = pipeline.serialize(msg)

      buffer(data)
      connection ! Write(data, Ack)

      context.become(buffering, discardOld = false)

    case msg: Server.ShutdownInitiated.type =>
      netClient ! msg

    case msg: ConnectionClosed =>
      log.info(s"Connection closed by {}.", msg)
      context.stop(self)
  })

  private def buffer(data: ByteString): Unit = {
    storage :+= data
    stored += data.size

    if (stored > maxToClientBufferSize) {
      log.warning(s"drop connection to [$connection] (buffer overrun)")
      context stop self

    } else if (stored > highWatermark) {
      log.debug(s"suspending reading")
      connection ! SuspendReading
      suspended = true
    }
  }

  private def acknowledge(): Unit = {
    require(storage.nonEmpty, "storage was empty")

    val size = storage.head.size
    stored -= size

    storage = storage.tail

    if (suspended && stored < lowWatermark) {
      log.debug("resuming reading")
      connection ! ResumeReading
      suspended = false
    }

    if (storage.isEmpty) {
      if (closing) context stop self
      else context.unbecome()
    }
    else connection ! Write(storage.head, Ack)
  }
}

class MsgHandlerPipeline(implicit byteOrder: ByteOrder, log: LoggingAdapter) {
  private[this] val intFramed = new IntFramedPipeline()

  def unserialize(data: ByteString) = intFramed.fromFramedData(data).map { frame =>
    Parsing.parse(frame.data).leftMap(err => s"Cannot decode $frame into message: $err")
  }

  def serialize(data: MsgHandler.Server2Client) =
    data |> Serializing.serialize |> Frame |> intFramed.withFrameSize
}