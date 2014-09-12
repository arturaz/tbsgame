package app.models.world

case class Bounds(x: Range, y: Range) {
  lazy val points = for {
    xCoord <- Stream.range(x.start, x.end + 1)
    yCoord <- Stream.range(y.start, y.end + 1)
  } yield Vect2(xCoord, yCoord)

  lazy val perimeter =
    Stream.range(x.start, x.end + 1).map(Vect2(_, y.start)) append
    Stream.range(y.start + 1, y.end + 1).map(Vect2(x.end, _)) append
    Stream.range(x.end - 1, x.start - 1, -1).map(Vect2(_, y.end)) append
    Stream.range(y.end - 1, y.start, -1).map(Vect2(x.start, _))

  lazy val center = Vect2(x.start + x.size / 2, y.start + y.size / 2)

  def contains(point: Vect2) = x.contains(point.x) && y.contains(point.y)

  def intersects(bounds: Bounds) = ! (
    x.end < bounds.x.start || y.end < bounds.y.start ||
    x.start > bounds.x.end || y.start > bounds.y.end
  )

  def +(v: Vect2) = Bounds(v.x + x.start to v.x + x.end, v.y + y.start to v.y + x.end)
  def -(v: Vect2) = this + -v

  override def toString =
    s"Bounds(${x.start}..${x.end} (${x.size}), ${y.start}..${y.end} (${y.size}))"
}

object Bounds {
  def apply(position: Vect2, size: Vect2): Bounds = Bounds(
    position.x to (position.x + size.x),
    position.y to (position.y + size.y)
  )
}
