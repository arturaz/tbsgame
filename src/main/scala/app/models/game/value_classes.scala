package app.models.game

import utils.IntValueClass

case class Actions(value: Int) extends AnyVal with IntValueClass[Actions] {
  override def self(v: Int) = Actions(value)
}
object Actions {
  implicit object Numeric extends IntValueClass.Numeric[Actions] {
    override def fromInt(x: Int) = Actions(x)
  }
}
