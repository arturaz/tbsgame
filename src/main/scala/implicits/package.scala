import utils.CompositeOrdering

import scala.reflect.ClassTag
import scala.util.Random

/**
 * Created by arturas on 2014-09-11.
 */
package object implicits {
  implicit class OptionExts[A](val o: Option[A]) extends AnyVal {
    @inline def fold2[B](ifEmpty: => B, ifSome: A => B) = o.fold(ifEmpty)(ifSome)
  }

  implicit class BoolExts(val b: Boolean) extends AnyVal {
    @inline def opt[A](onTrue: => A): Option[A] = if (b) Some(onTrue) else None
  }

  implicit class RangeExts(val r: Range) extends AnyVal {
    def random =
      if (r.size < 2) r.start
      else r.start + Random.nextInt(r.end - r.start)
  }

  implicit class RandomExts(val r: Random) extends AnyVal {
    def double(from: Double, to: Double) = (to - from) * r.nextDouble() + from
  }

  implicit class IndexedSeqExts[A](val is: IndexedSeq[A]) extends AnyVal {
    def random = if (is.isEmpty) None else Some(is(Random.nextInt(is.size)))
  }

  implicit class OrderingOps[T](val ord: Ordering[T]) extends AnyVal {
    def orElse(ord2: Ordering[T]) = new CompositeOrdering[T](ord, ord2)
  }

  implicit class AnyExts[A](val a: A) extends AnyVal {
    @inline def ===(a1: A) = a == a1
    @inline def =/=(a1: A) = a != a1
    @inline def |>[B](f: A => B) = f(a)
    @inline def left[B]: Either[A, B] = Left(a)
    @inline def right[B]: Either[B, A] = Right(a)
    @inline def mapVal[B](f: A => B) = f(a)
    @inline def tap(f: A => Unit) = { f(a); a }
    @inline def cast[B : ClassTag]: Option[B] =
      a match { case b: B => Some(b); case _ => None }
  }
}
