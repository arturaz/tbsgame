package app.models.world

object TileRange {
  def reachable(origin: Vect2, range: Int, includeSelf: Boolean=true) = {
    for {
      x <- Stream.range(-range, range + 1)
      curY = range - math.abs(x)
      y <- Stream.range(-curY, curY + 1)
      if includeSelf || x != origin.x && y != origin.y
    } yield Vect2(x, y)
  }

  def apply(range: Int): TileRange = {
    require(range >= 0, s"range >= 0: $range")
    new TileRange(range)
  }
}

class TileRange private (val range: Int) extends AnyVal with Ordered[TileRange] {
  def +(v: Int) = TileRange(range + v)
  def -(v: Int) = this + -v

  def reachable(origin: Vect2) = TileRange.reachable(origin, range)
  override def compare(that: TileRange) = range.compareTo(that.range)
}
