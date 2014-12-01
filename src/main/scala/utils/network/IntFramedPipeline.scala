package utils.network

import java.nio.ByteOrder

import akka.util.ByteString
import utils.network.IntFramedPipeline.Frame

class IntFramedPipeline(implicit byteOrder: ByteOrder)
extends Pipeline[ByteString, Vector[Frame], Frame, ByteString] {
  import IntFramedPipeline._

  private[this] var buffer = ByteString.empty

  def fromClient(data: ByteString): Vector[Frame] = {
    buffer ++= data
//    Log.debug(s"Received ${data.size} bytes ($data).")
    val (newBuf, frames) = process(buffer)
    buffer = newBuf
    frames
  }

  def toClient(data: Frame) = {
    val bb = ByteString.newBuilder
    bb.putInt(data.data.size)
    bb ++= data.data
    val result = bb.result()
//    Log.debug(s"Encoded frame of size ${data.data.size} ($data) to $result")
    result
  }

  private[this] def process(
    buffer: Buffer, frames: Vector[Frame]=Vector.empty
    ): (Buffer, Vector[Frame]) = {
    if (buffer.length >= frameLengthSize) {
      val iter = buffer.iterator
      val frameSize = iter.getInt
      val rest = iter.toByteString
      if (rest.length >= frameSize) {
        val (frame, newBuf) = rest.splitAt(frameSize)
//        Log.debug(
//          s"Frame of size $frameSize received, rest of the buffer is ${
//            newBuf.size} bytes"
//        )
        return process(newBuf, frames :+ Frame(frame))
      }
      else {
//        Log.debug(s"Still need ${frameSize - rest.length} bytes for frame.")
      }
    }

    (buffer, frames)
  }
}

object IntFramedPipeline {
  val frameLengthSize = 4
  case class Frame(data: ByteString) extends AnyVal
  type Buffer = ByteString
}
