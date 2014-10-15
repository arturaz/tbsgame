package utils.network

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
      pipeline.fromClient(self.fromClient(data))
    override def toClient(data: FromServerIn) =
      pipeline.toClient(self.toClient(data))
  }
}
