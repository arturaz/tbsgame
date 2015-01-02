package app.actors

import java.nio.ByteOrder

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.{LoggingAdapter, LoggingReceive}
import akka.io.Tcp.{ConnectionClosed, Received, Write}
import akka.util.ByteString
import app.actors.NetClient.Msgs.{FromClient, FromServer}
import utils.network.{CodedFramePipeline, IntFramedPipeline, Pipeline}

/**
 * Created by arturas on 2014-10-15.
 */
class MsgHandler(
  connection: ActorRef, netClientProps: ActorRef => Props
)(implicit byteOrder: ByteOrder, log: LoggingAdapter) extends Actor with ActorLogging {
  private[this] val netClient = context.actorOf(netClientProps(self), "net-client")
  context.watch(netClient)

  private[this] val pipeline = new MsgHandlerPipeline

  override def receive = LoggingReceive {
    case Received(data) => pipeline.fromClient(data).foreach(msg => netClient ! msg)
    case msg: FromServer => connection ! Write(pipeline.toClient(msg))
    case msg: ConnectionClosed =>
      log.info(s"Connection closed.")
      context.stop(self)
  }
}

class MsgHandlerPipeline(implicit byteOrder: ByteOrder, log: LoggingAdapter)
extends Pipeline[ByteString, Vector[FromClient], FromServer, ByteString] {
  private[this] val intFramed = new IntFramedPipeline()
  private[this] val coded = new CodedFramePipeline(
    ProtobufCoding.Parsing.parse, ProtobufCoding.Serializing.serialize
  )

  override def fromClient(data: ByteString) = intFramed.fromClient(data).map { frame =>
    coded.fromClient(frame).fold(
      err =>
        throw new IllegalArgumentException(s"Cannot decode $frame into message: $err"),
      identity
    )
  }

  override def toClient(data: FromServer) = intFramed.toClient(coded.toClient(data))
}