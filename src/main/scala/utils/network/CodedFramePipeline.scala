package utils.network

import akka.util.ByteString
import utils.network.IntFramedPipeline.Frame

import scala.reflect.ClassTag
import scalaz.\/

class CodedFramePipeline[In, Out : ClassTag](
  decoder: ByteString => String \/ In, encoder: Out => ByteString
) extends Pipeline[Frame, String \/ In, Out, Frame] {
  override def fromClient(data: Frame) = decoder(data.data)
  override def toClient(data: Out) = Frame(encoder(data))
}
