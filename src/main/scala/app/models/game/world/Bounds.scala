package app.models.game.world

import implicits._
import utils.IntValueClass

case class Bounds(x: Range.Inclusive, y: Range.Inclusive) {
  require(
    x.size > 0 && y.size > 0,
    s"Bounds size must be > 0: x=${rstr(x)} y=${rstr(y)}"
  )

  def start = Vect2(x.start, y.start)
  def end = Vect2(x.end, y.end)

  def random = Vect2(x.random, y.random)

  def points = for {
    xCoord <- Iterator.range(x.start, x.end + 1)
    yCoord <- Iterator.range(y.start, y.end + 1)
  } yield Vect2(xCoord, yCoord)

  def corners = Iterator(
    Vect2(x.start, y.start), Vect2(x.end, y.start), Vect2(x.end, y.end),
    Vect2(x.start, y.end)
  )

  /* N-th perimeter, where n >= 0. If n > 0 returns a perimeter that is
     n tiles away from these bounds */
  def perimeterN(n: Int) = {
    require(n >= 0, s"n should be positive, $n given")
    val (xS, xE, yS, yE) = (x.start - n, x.end + n, y.start - n, y.end + n)

    Iterator.range(xS, xE + 1).map(Vect2(_, yS)) ++
    Iterator.range(yS + 1, yE + 1).map(Vect2(xE, _)) ++
    Iterator.range(xE - 1, xS - 1, -1).map(Vect2(_, yE)) ++
    Iterator.range(yE - 1, yS, -1).map(Vect2(xS, _))
  }
  def perimeter = perimeterN(0)

  lazy val center = Vect2(x.start + x.size / 2, y.start + y.size / 2)

  def contains(point: Vect2): Boolean =
    x.start <= point.x && point.x <= x.end &&
    y.start <= point.y && point.y <= y.end

  def contains(bounds: Bounds): Boolean =
    x.start <= bounds.x.start && bounds.x.end <= x.end &&
    y.start <= bounds.y.start && bounds.y.end <= y.end

  def withinDistance[A <: Ordered[A]](distance: A, f: Vect2 => A): Boolean =
    perimeter.exists(f(_) <= distance)
  def withinDistance(point: Vect2, distance: TileDistance): Boolean =
    withinDistance(distance, point.tileDistance(_))
  def withinDistance(point: Vect2, distance: RadialDistance): Boolean =
    withinDistance(distance, point.radialDistance(_))
  def tileDistance(point: Vect2) = point.tileDistance(this)

  def intersects(bounds: Bounds) = ! (
    x.end < bounds.x.start || y.end < bounds.y.start ||
    x.start > bounds.x.end || y.start > bounds.y.end
  )

  def intersection(bounds: Bounds): Option[Bounds] = {
    val xStart = x.start max bounds.x.start
    val xEnd = x.end min bounds.x.end
    val yStart = y.start max bounds.y.start
    val yEnd = y.end min bounds.y.end

    if (xStart != xEnd && yStart != yEnd) Some(Bounds(xStart to xEnd, yStart to yEnd))
    else None
  }

  def join(bounds: Bounds) = Bounds(
    x.start.min(bounds.x.start) to x.end.max(bounds.x.max),
    y.start.min(bounds.y.start) to y.end.max(bounds.y.max)
  )

  def +(v: Vect2) = Bounds(v.x + x.start to v.x + x.end, v.y + y.start to v.y + y.end)
  def -(v: Vect2) = this + -v

  def expandBy(v: Int) = Bounds(x.start - v to x.end + v, y.start - v to y.end + v)

  override def toString = {
    s"Bounds(${rstr(x)} (${x.size}), ${rstr(y)} (${y.size}))"
  }

  private[this] def rstr(r: Range.Inclusive) = s"${r.start}..${r.end}"
}

object Bounds {
  def apply(position: Vect2, size: Vect2): Bounds = {
    Bounds(
      position.x to (position.x + size.x - 1),
      position.y to (position.y + size.y - 1)
    )
  }
}
