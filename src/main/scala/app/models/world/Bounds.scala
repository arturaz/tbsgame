package app.models.world

case class Bounds(x: Range, y: Range) {
  lazy val points = for {
    xCoord <- Iterator.range(x.start, x.end + 1)
    yCoord <- Iterator.range(y.start, y.end + 1)
  } yield Vect2(xCoord, yCoord)

  lazy val corners = Iterator(
    Vect2(x.start, y.start), Vect2(x.end, y.start), Vect2(x.end, y.end),
    Vect2(x.start, y.end)
  )

  lazy val perimeter =
    Iterator.range(x.start, x.end + 1).map(Vect2(_, y.start)) ++
    Iterator.range(y.start + 1, y.end + 1).map(Vect2(x.end, _)) ++
    Iterator.range(x.end - 1, x.start - 1, -1).map(Vect2(_, y.end)) ++
    Iterator.range(y.end - 1, y.start, -1).map(Vect2(x.start, _))

  lazy val center = Vect2(x.start + x.size / 2, y.start + y.size / 2)

  def contains(point: Vect2) = x.contains(point.x) && y.contains(point.y)
  def withinTileDistance(point: Vect2, distance: TileDistance) =
    perimeter.exists(point.tileDistance(_) <= distance)
  def tileDistance(point: Vect2) = point.tileDistance(this)

  def intersects(bounds: Bounds) = ! (
    x.end < bounds.x.start || y.end < bounds.y.start ||
    x.start > bounds.x.end || y.start > bounds.y.end
  )

  def join(bounds: Bounds) = Bounds(
    x.start.min(bounds.x.start) to x.end.max(bounds.x.max),
    y.start.min(bounds.y.start) to y.end.max(bounds.y.max)
  )

  def +(v: Vect2) = Bounds(v.x + x.start to v.x + x.end, v.y + y.start to v.y + y.end)
  def -(v: Vect2) = this + -v

  def expandBy(v: Int) = Bounds(x.start - v to x.end + v, y.start - v to y.end + v)

  override def toString =
    s"Bounds(${x.start}..${x.end} (${x.size}), ${y.start}..${y.end} (${y.size}))"
}

object Bounds {
  def apply(position: Vect2, size: Vect2): Bounds = Bounds(
    position.x until (position.x + size.x),
    position.y until (position.y + size.y)
  )
}
