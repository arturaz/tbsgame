package utils

import implicits._
import scalaz._, Scalaz._

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

trait IntValueClass[A <: IntValueClass[A]] extends Any with Ordered[A] {
  def value: Int
  def self(v: Int): A

  def +(v: A) = self(value + v.value)
  def -(v: A) = self(value - v.value)
  def *(v: A) = self(value * v.value)
  def /(v: A) = self(value / v.value)
  def unary_- = self(-value)

  def isZero = value === 0
  def isNotZero = ! isZero
  def isPositive = value > 0
  def isNegative = value < 0

  def min(v: A) = self(value min v.value)
  def max(v: A) = self(value max v.value)

  override def compare(that: A) = value.compareTo(that.value)
}

class IntVCInclusiveRange[A <: IntValueClass[A]](
  from: A, to: A, step: Int=1
) extends Range.Inclusive(from.value, to.value, 1)