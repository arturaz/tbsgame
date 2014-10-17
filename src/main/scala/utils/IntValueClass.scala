package utils

object IntValueClass {
  trait Numeric[A <: IntValueClass[A]] extends scala.Numeric[A] {
    override def plus(x: A, y: A) = x + y
    override def toDouble(x: A) = x.value.toDouble
    override def toFloat(x: A) = x.value.toFloat
    override def toInt(x: A) = x.value
    override def negate(x: A) = -x
    override def toLong(x: A) = x.value.toLong
    override def times(x: A, y: A) = x * y
    override def minus(x: A, y: A) = x - y
    override def compare(x: A, y: A) = x compare y
  }
}

trait IntValueClass[A <: IntValueClass[_]] extends Any with Ordered[A] {
  def value: Int
  def self(v: Int): A

  def +(v: A) = self(value + v.value)
  def -(v: A) = self(value - v.value)
  def *(v: A) = self(value * v.value)
  def /(v: A) = self(value / v.value)
  def unary_- = self(-value)

  def isZero = value == 0
  def isNotZero = ! isZero

  override def compare(that: A) = value.compareTo(that.value)
}
