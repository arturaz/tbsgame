package utils

import spire.math.Rational
import implicits._

object RationalValueClass {
  trait Numeric[A <: RationalValueClass[A]] extends scala.Numeric[A] {
    override def plus(x: A, y: A) = x + y
    override def toFloat(x: A) = x.value.toFloat
    override def toDouble(x: A) = x.value.toDouble
    override def toInt(x: A) = x.value.toInt
    override def negate(x: A) = -x
    override def toLong(x: A) = x.value.toLong
    override def times(x: A, y: A) = x * y
    override def minus(x: A, y: A) = x - y
    override def compare(x: A, y: A) = x compare y
  }
}

trait RationalValueClass[A <: RationalValueClass[A]] extends Any with Ordered[A] {
  def value: Rational
  def self(v: Rational): A

  def +(v: A) = self(value + v.value)
  def -(v: A) = self(value - v.value)
  def *(v: A) = self(value * v.value)
  def /(v: A) = self(value / v.value)
  def unary_- = self(-value)

  def isZero = value === 0
  def isNotZero = ! isZero

  def min(v: A) = self(value min v.value)
  def max(v: A) = self(value max v.value)

  def sqr = self(value * value)

  override def compare(that: A) = value.compareTo(that.value)
}
