package app.models.game.world

import app.models.game.Actions
import utils.{IntVCInclusiveRange, DoubleValueClass, IntValueClass}

import scala.util.Random

case class Resources(value: Int) extends AnyVal with IntValueClass[Resources] {
  override def self(v: Int) = Resources(v)
}
object Resources {
  implicit object Numeric extends IntValueClass.Numeric[Resources] {
    override def fromInt(x: Int) = Resources(x)
  }
}

/* Victory Points */
case class VPS(value: Int) extends AnyVal with IntValueClass[VPS] {
  override def self(v: Int) = VPS(v)
}
object VPS {
  implicit object Numeric extends IntValueClass.Numeric[VPS] {
    override def fromInt(x: Int) = VPS(x)
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

case class Atk(value: Int) extends AnyVal with IntValueClass[Atk] {
  override def self(v: Int) = Atk(v)
}
object Atk {
  implicit object Numeric extends IntValueClass.Numeric[Atk] {
    override def fromInt(x: Int) = Atk(x)
  }
}
case class AtkRange(from: Atk, to: Atk) extends IntVCInclusiveRange(from, to)

case class AtkSpread(value: Double) extends AnyVal with DoubleValueClass[AtkSpread] {
  override def self(v: Double) = AtkSpread(v)
}
object AtkSpread {
  implicit object Numeric extends DoubleValueClass.Numeric[AtkSpread] {
    override def fromInt(x: Int) = AtkSpread(x)
  }
}

case class SpawnerStr(value: Int) extends AnyVal with IntValueClass[SpawnerStr] {
  override def self(v: Int) = SpawnerStr(v)
}
object SpawnerStr {
  implicit object Numeric extends IntValueClass.Numeric[SpawnerStr] {
    override def fromInt(x: Int) = SpawnerStr(x)
  }
}

case class Attacks(value: Int) extends AnyVal with IntValueClass[Attacks] {
  override def self(v: Int) = Attacks(v)
}
object Attacks {
  implicit object Numeric extends IntValueClass.Numeric[Attacks] {
    override def fromInt(x: Int) = Attacks(x)
  }
}

case class Level(value: Int) extends AnyVal with IntValueClass[Level] {
  override def self(v: Int) = Level(v)
}
object Level {
  implicit object Numeric extends IntValueClass.Numeric[Level] {
    override def fromInt(x: Int) = Level(x)
  }
}

case class XP(value: Int) extends AnyVal with IntValueClass[XP] {
  override def self(v: Int) = XP(v)
}
object XP {
  implicit object Numeric extends IntValueClass.Numeric[XP] {
    override def fromInt(x: Int) = XP(x)
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


case class Movement private (value: Int) extends IntValueClass[Movement] {
  override def self(v: Int) = new Movement(v)
  def tileValue = value / Movement.Multiplier
}
object Movement {
  val Multiplier = 1000
  val zero = fromAbsolute(0)

  def fromTiles(tiles: Int) = fromAbsolute(tiles * Multiplier)
  def fromAbsolute(value: Int) = Movement(value)

  implicit object MovementNumeric extends IntValueClass.Numeric[Movement] {
    override def fromInt(x: Int) = Movement.fromAbsolute(x)
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
