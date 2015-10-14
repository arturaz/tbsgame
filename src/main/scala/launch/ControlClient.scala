package launch

import java.net.Socket
import java.nio.ByteOrder

import akka.event.LoggingAdapter
import akka.util.ByteString
import app.actors.NetClient
import app.protobuf.parsing.Parsing
import app.protobuf.serializing.Serializing
import utils.network.IntFramedPipeline
import utils.network.IntFramedPipeline.Frame

import scala.reflect.ClassTag
import scalaz._, Scalaz._
import scalaz.effect.IO

/**
 * Created by arturas on 2015-10-13.
 */
object ControlClient {
  def sendShutdown(implicit cfg: RTConfig, byteOrder: ByteOrder) =
    connectSendAndDisconnect[NetClient.Control.Out.GenericReply](NetClient.Control.In.Shutdown)

  def sendStatusReq(implicit cfg: RTConfig, byteOrder: ByteOrder) =
    connectSendAndDisconnect[NetClient.Control.Out.Status](NetClient.Control.In.Status)

  def connectSendAndDisconnect[Reply <: NetClient.Control.Out]
  (msg: NetClient.Control.In)
  (implicit cfg: RTConfig, byteOrder: ByteOrder, replyCT: ClassTag[Reply])
  : IO[String \/ Reply] =
    for {
      socket <- IO { new Socket("localhost", cfg.port.signed) }
      res <- sendAndReceive[Reply](socket, msg) ensuring IO(socket.close())
  } yield res

  def sendAndReceive[Reply <: NetClient.Control.Out]
  (socket: Socket, msg: NetClient.Control.In)
  (implicit cfg: RTConfig, byteOrder: ByteOrder, replyCT: ClassTag[Reply])
  : IO[String \/ Reply] = IO {
    val is = socket.getInputStream
    val os = socket.getOutputStream
    val msgWithKey = NetClient.Msgs.FromControlClient(cfg.controlKey, msg)
    val protoMsgBytes = Serializing.serializeControl(msgWithKey)
    val serialized = IntFramedPipeline.withFrameSize(Frame(protoMsgBytes))
    os.write(serialized.toArray)
    os.flush()
    val frameLengthArr = new Array[Byte](IntFramedPipeline.frameLengthSize)
    is.read(frameLengthArr, 0, frameLengthArr.length)
    val frameLength = ByteString(frameLengthArr).iterator.getInt
    val frameArr = new Array[Byte](frameLength)
    is.read(frameArr, 0, frameArr.length)
    val frame = ByteString(frameArr)
    val res = Parsing.parseFromControlServer(frame).flatMap {
      case r: Reply => r.right
      case other => s"Expected $replyCT, but got $other".left
    }
    res
  }
}
