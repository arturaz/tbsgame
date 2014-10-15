package utils.network

import akka.util.ByteString
import utils.network.IntFramedPipeline.Frame

import scala.reflect.ClassTag

class CodedFramePipeline[In, Out : ClassTag](
  decoder: ByteString => Either[String, In], encoder: Out => ByteString
) extends Pipeline[Frame, Either[String, In], Out, Frame] {
  override def fromClient(data: Frame) = decoder(data.data)
  override def toClient(data: Out) = Frame(encoder(data))
}
