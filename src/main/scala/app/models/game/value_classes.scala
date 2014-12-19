package app.models.game

import utils.{DoubleValueClass, IntValueClass}

case class Actions(value: Int) extends AnyVal with IntValueClass[Actions] {
  override def self(v: Int) = Actions(v)
}
object Actions {
  implicit object Numeric extends IntValueClass.Numeric[Actions] {
    override def fromInt(x: Int) = Actions(x)
  }
}

case class Population(value: Int) extends AnyVal with IntValueClass[Population] {
  override def self(v: Int) = Population(v)
}
object Population {
  implicit object Numeric extends IntValueClass.Numeric[Population] {
    override def fromInt(x: Int) = Population(x)
  }
}

case class Percentage(value: Double) extends AnyVal with DoubleValueClass[Percentage] {
  def self(v: Double) = Percentage(v)
}
object Percentage {
  implicit object Numeric extends DoubleValueClass.Numeric[Percentage] {
    override def fromInt(x: Int) = Percentage(x)
  }
}

case class ObjectCount(value: Int) extends AnyVal with IntValueClass[ObjectCount] {
  def self(v: Int) = ObjectCount(v)
}
object ObjectCount {
  implicit object Numeric extends IntValueClass.Numeric[ObjectCount] {
    override def fromInt(x: Int) = ObjectCount(x)
  }
}

case class Winner(team: Team) extends AnyVal