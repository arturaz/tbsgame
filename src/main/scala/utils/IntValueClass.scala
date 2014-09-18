package utils

/**
 * Created by arturas on 2014-09-18.
 */
trait IntValueClass[A <: IntValueClass[_]] extends Any with Ordered[A] {
  def value: Int
  def self(v: Int): A

  def +(v: A) = self(value + v.value)
  def -(v: A) = self(value - v.value)
  def unary_-(v: A) = self(-value)
  override def compare(that: A) = value.compareTo(that.value)
}
