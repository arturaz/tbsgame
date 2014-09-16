import utils.CompositeOrdering

import scala.util.Random

/**
 * Created by arturas on 2014-09-11.
 */
package object implicits {
  implicit class RangeExts(val r: Range) extends AnyVal {
    def random = r.start + Random.nextInt(r.end - r.start)
  }

  implicit class IndexedSeqExts[A](val is: IndexedSeq[A]) extends AnyVal {
    def random = if (is.isEmpty) None else Some(is(Random.nextInt(is.size)))
  }

  implicit class OrderingOps[T](val ord: Ordering[T]) extends AnyVal {
    def orElse(ord2: Ordering[T]) = new CompositeOrdering[T](ord, ord2)
  }
}
