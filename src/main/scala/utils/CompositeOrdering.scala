package utils

import implicits._
import scalaz._, Scalaz._

final class CompositeOrdering[T](
  val ord1: scala.Ordering[T], val ord2: scala.Ordering[T]
) extends scala.Ordering[T] {
  def compare( x: T, y: T ) = {
    val comp = ord1.compare( x, y )
    if ( comp =/= 0 ) comp else ord2.compare( x, y )
  }
}

object CompositeOrdering {
  def apply[T](orderings: scala.Ordering[T]*) = orderings reduceLeft (_ orElse _)
}