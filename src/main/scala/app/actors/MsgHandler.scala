package app.actors

import java.nio.ByteOrder

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.{LoggingAdapter, LoggingReceive}
import akka.io.Tcp._
import akka.util.ByteString
import app.actors.NetClient.Msgs.{FromClient, FromServer}
import app.protobuf.parsing.Parsing
import app.protobuf.serializing.Serializing
import utils.network.{CodedFramePipeline, IntFramedPipeline, Pipeline}
import implicits._
import scalaz._, Scalaz._

/**
 * Created by arturas on 2014-10-15.
 */
object MsgHandler {
  private case object Ack extends Event
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
    case Received(data) => pipeline.fromClient(data).foreach {
      case -\/(err) => log.error(err)
      case \/-(msg) => netClient ! msg
    }
  }

  private[this] val buffering = LoggingReceive(fromClient orElse {
    case msg: FromServer =>
      buffer(pipeline.toClient(msg))

    case Ack =>
      acknowledge()

    case msg: ConnectionClosed =>
      log.info(s"closing = true by {}.", msg)
      closing = true
  })

  override val receive = LoggingReceive(fromClient orElse {
    case msg: FromServer =>
      val data = pipeline.toClient(msg)

      buffer(data)
      connection ! Write(data, Ack)

      context.become(buffering, discardOld = false)

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

class MsgHandlerPipeline(implicit byteOrder: ByteOrder, log: LoggingAdapter)
extends Pipeline[ByteString, Vector[String \/ FromClient], FromServer, ByteString] {
  private[this] val intFramed = new IntFramedPipeline()
  private[this] val coded = new CodedFramePipeline(Parsing.parse, Serializing.serialize)

  override def fromClient(data: ByteString) = intFramed.fromClient(data).map { frame =>
    coded.fromClient(frame).leftMap(err => s"Cannot decode $frame into message: $err")
  }

  override def toClient(data: FromServer) =
    data |> coded.toClient |> intFramed.toClient
}