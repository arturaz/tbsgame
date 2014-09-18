package app.models.world

import utils.IntValueClass

object TileDistance {
  def reachable(origin: Vect2, range: Int, includeSelf: Boolean=true) = {
    for {
      x <- Stream.range(-range, range + 1)
      curY = range - math.abs(x)
      y <- Stream.range(-curY, curY + 1)
      if includeSelf || x != origin.x && y != origin.y
    } yield Vect2(x, y)
  }

  def apply(value: Int): TileDistance = {
    require(value >= 0, s"value >= 0: $value")
    new TileDistance(value)
  }

//  implicit object Ordering extends Ordering[TileDistance] {
//    override def compare(x: TileDistance, y: TileDistance) = x.compare(y)
//  }
}

class TileDistance private (val value: Int) extends AnyVal
with IntValueClass[TileDistance] {
  def reachable(origin: Vect2) = TileDistance.reachable(origin, value)
  override def self(v: Int) = TileDistance(v)
}
