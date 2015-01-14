package app.models.game.world

import utils.{FloatValueClass, DoubleValueClass, IntValueClass}
import implicits._

object TileDistance {
  def reachable(origin: Vect2, range: Int, includeSelf: Boolean=true) = {
    for {
      x <- Stream.range(-range, range + 1)
      curY = range - math.abs(x)
      y <- Stream.range(-curY, curY + 1)
      if includeSelf || x =/= origin.x && y =/= origin.y
    } yield Vect2(x, y)
  }

  def apply(value: Int): TileDistance = {
    require(value >= 0, s"value < 0: $value")
    new TileDistance(value)
  }
}

class TileDistance private (val value: Int) extends AnyVal
with IntValueClass[TileDistance] {
  def reachable(origin: Vect2) = TileDistance.reachable(origin, value)
  override def self(v: Int) = TileDistance(v)
}

class RadialDistance private (val value: Float) extends AnyVal
with FloatValueClass[RadialDistance]
{
  override def self(v: Float) = RadialDistance(v)
}

object RadialDistance {
  // Radial distances that look best in certain tile distances
  val One = RadialDistance(1.5f)
  val Two = RadialDistance(2.5f)
  val Three = RadialDistance(3.5f)
  val Four = RadialDistance(4.3f)
  val Five = RadialDistance(5.25f)
  val Six = RadialDistance(6.5f)
  val Seven = RadialDistance(7.5f)
  val Eight = RadialDistance(8.3f)
  val Nine = RadialDistance(9.5f)
  val Ten = RadialDistance(10.5f)

  def apply(value: Float): RadialDistance = {
    require(value >= 0, s"value < 0: $value")
    new RadialDistance(value)
  }
}