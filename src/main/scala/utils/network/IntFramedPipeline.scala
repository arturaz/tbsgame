package utils.network

import java.nio.ByteOrder

import akka.event.LoggingAdapter
import akka.util.ByteString
import utils.Hex
import utils.network.IntFramedPipeline.Frame

import scala.annotation.tailrec

class IntFramedPipeline(implicit byteOrder: ByteOrder) {
  import IntFramedPipeline._

  private[this] var buffer = ByteString.empty

  def fromFramedData(data: ByteString): Vector[Frame] = {
    buffer ++= data
    val (newBuf, frames) = process(buffer)
    buffer = newBuf
    frames
  }

  def withFrameSize(data: Frame) = IntFramedPipeline.withFrameSize(data)

  @tailrec private[this] def process(
    buffer: Buffer, frames: Vector[Frame]=Vector.empty
  ): (Buffer, Vector[Frame]) = {
    def defaultRet = (buffer, frames)

    if (buffer.length >= frameLengthSize) {
      val iter = buffer.iterator
      val frameSize = iter.getInt
      val rest = iter.toByteString
      if (rest.length >= frameSize) {
        val (frame, newBuf) = rest.splitAt(frameSize)
        process(newBuf, frames :+ Frame(frame))
      }
      else defaultRet
    }
    else defaultRet
  }
}

object IntFramedPipeline {
  val frameLengthSize = 4
  case class Frame(data: ByteString) extends AnyVal
  type Buffer = ByteString

  def withFrameSize(data: Frame)(implicit byteOrder: ByteOrder) = {
    val bb = ByteString.newBuilder
    bb.putInt(data.data.size)
    bb ++= data.data
    val result = bb.result()
    result
  }
}
