package utils.actors

import akka.actor.{ActorLogging, Actor, ActorRef}
import akka.event.LoggingReceive
import akka.util.ByteString

import scala.reflect.ClassTag

object CodedFrameProxy {
  case class Init(intFramedProxy: ActorRef, decodedMessageHandler: ActorRef)
}

class CodedFrameProxy[In, Out : ClassTag](
  decoder: ByteString => Either[String, In], encoder: Out => ByteString
) extends Actor with ActorLogging with ProxyUnknown {
  import CodedFrameProxy._

  override def receive = waitingForInit

  private[this] def waitingForInit: Receive = LoggingReceive {
    case init: Init =>
      context.watch(init.intFramedProxy)
      context.watch(init.decodedMessageHandler)
      context.become(initialized(init))
  }

  private[this] def initialized(init: Init): Receive = LoggingReceive(({
    case newInit: Init =>
      log.warning(s"Received new init ($newInit) while already initialized with $init")
    case IntFramedProxy.Frame(data) =>
      decoder(data).fold(
        err => log.warning(s"Cannot decode $data: $err"),
        in => init.decodedMessageHandler.tell(in, sender())
      )
    case msg: Out =>
      init.intFramedProxy.tell(IntFramedProxy.Frame(encoder(msg)), sender())
  }: Receive) orElse proxyUnknown(init.intFramedProxy, init.decodedMessageHandler))
}
