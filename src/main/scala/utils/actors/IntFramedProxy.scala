package utils.actors

import java.nio.ByteOrder

import akka.actor.{ActorLogging, ActorRef, Actor}
import akka.event.LoggingReceive
import akka.io.Tcp.{Write, Received}
import akka.util.ByteString


object IntFramedProxy {
  val frameLengthSize = 4

  case class Frame(data: ByteString)
  case class Init(connection: ActorRef, frameReceiver: ActorRef)
}

class IntFramedProxy(implicit byteOrder: ByteOrder) extends Actor with ActorLogging
with ProxyUnknown {
  import utils.actors.IntFramedProxy._

  private[this] var buffer = ByteString.empty

  override def receive = waitingForInit

  private[this] def waitingForInit: Receive = LoggingReceive {
    case init: Init =>
      log.debug(s"Initializing with {}", init)
      context.watch(init.connection)
      context.watch(init.frameReceiver)
      context.become(waitingForLength(init))
  }

  private[this] def common(init: Init): Receive = {
    case newInit: Init =>
      log.warning(s"Received new init ($newInit) while already initialized with $init")
    case Frame(data) =>
      log.debug(
        "Received frame of size {}, writing to connection {}", data.size, init.connection
      )
      val bb = ByteString.newBuilder
      bb.putInt(data.size)
      bb ++= data
      init.connection ! Write(bb.result())
  }

  private[this] def commonPost(init: Init): Receive =
    proxyUnknown(init.connection, init.frameReceiver)

  private[this] def waitingForLength(init: Init): Receive = {
    log.debug("Waiting for frame length with {}", init)
    LoggingReceive(
      common(init) orElse ({
        case Received(data) =>
          buffer ++= data
          log.debug(
            "Received data of size {}, total buffer size {}", data.size, buffer.size
          )
          if (buffer.length >= frameLengthSize) {
            val iter = buffer.iterator
            val frameSize = iter.getInt
            buffer = iter.toByteString
            context.become(waitingForFrame(init, frameSize))
          }
      }: Receive) orElse commonPost(init)
    )
  }

  private[this] def waitingForFrame(init: Init, size: Int): Receive = {
    log.debug("Waiting for frame of size {} with {}", size, init)
    LoggingReceive(
      common(init) orElse ({
        case Received(data) =>
          buffer ++= data
          log.debug(
            "Received data of size {}, total buffer size {}, needed {}, left {}",
            data.size, buffer.size, size, size - buffer.size
          )
          if (buffer.length >= size) {
            val (frame, rest) = buffer.splitAt(size)
            buffer = rest
            log.debug("Frame received, rest of the buffer is {} bytes", buffer.size)
            init.frameReceiver.tell(Frame(frame), sender())
            context.become(waitingForLength(init))
          }
      }: Receive) orElse commonPost(init)
    )
  }
}