package utils

import implicits._

final class CompositeOrdering[T](
  val ord1: Ordering[T], val ord2: Ordering[T]
) extends Ordering[T] {
  def compare( x: T, y: T ) = {
    val comp = ord1.compare( x, y )
    if ( comp =/= 0 ) comp else ord2.compare( x, y )
  }
}

object CompositeOrdering {
  def apply[T](orderings: Ordering[T]*) = orderings reduceLeft (_ orElse _)
}