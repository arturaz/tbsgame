package app.models.game.world

import utils.{DoubleValueClass, IntValueClass}

import scala.util.Random

case class Resources(value: Int) extends AnyVal with IntValueClass[Resources] {
  override def self(v: Int) = Resources(v)
}
object Resources {
  implicit object Numeric extends IntValueClass.Numeric[Resources] {
    override def fromInt(x: Int) = Resources(x)
  }
}

case class WarpTime(value: Int) extends AnyVal with IntValueClass[WarpTime] {
  override def self(v: Int) = WarpTime(v)
}
object WarpTime {
  implicit object Numeric extends IntValueClass.Numeric[WarpTime] {
    override def fromInt(x: Int) = WarpTime(x)
  }
}

case class HP(value: Int) extends AnyVal with IntValueClass[HP] {
  override def self(v: Int) = HP(v)
}
object HP {
  implicit object Numeric extends IntValueClass.Numeric[HP] {
    override def fromInt(x: Int) = HP(x)
  }
}

case class RectDistance(value: Int) extends AnyVal with IntValueClass[RectDistance] {
  override def self(v: Int) = RectDistance(v)

  def extend(bounds: Bounds) = Bounds(
    bounds.x.start - value to bounds.x.end + value,
    bounds.y.start - value to bounds.y.end + value
  )
}
object RectDistance {
  implicit object Numeric extends IntValueClass.Numeric[RectDistance] {
    override def fromInt(x: Int) = RectDistance(x)
  }
}


case class Chance(value: Double) extends AnyVal with DoubleValueClass[Chance] {
  override def self(v: Double) = Chance(v)

  def struck = Random.nextDouble() <= value
}
object Chance {
  implicit object Numeric extends DoubleValueClass.Numeric[Chance] {
    override def fromInt(x: Int) = Chance(x)
  }
}
