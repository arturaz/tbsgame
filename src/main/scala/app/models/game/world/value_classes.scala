package app.models.game.world

import utils.IntValueClass

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
