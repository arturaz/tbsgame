package utils.network

import implicits._

/**
 * Created by arturas on 2014-10-15.
 */
trait Pipeline[FromClientIn, FromClientOut, FromServerIn, FromServerOut] { self =>
  def fromClient(data: FromClientIn): FromClientOut
  def toClient(data: FromServerIn): FromServerOut

  def compose[FCO2, FSO2](
    pipeline: Pipeline[FromClientOut, FCO2, FromServerOut, FSO2]
  ) = new Pipeline[FromClientIn, FCO2, FromServerIn, FSO2] {
    override def fromClient(data: FromClientIn) =
      data |> self.fromClient |> pipeline.fromClient
    override def toClient(data: FromServerIn) =
      data |> self.toClient |> pipeline.toClient
  }
}
