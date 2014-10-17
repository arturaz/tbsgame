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
