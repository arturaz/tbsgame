import java.nio.{ByteBuffer, ByteOrder}
import java.util.UUID

import akka.event.LoggingAdapter
import infrastructure.PrefixedLoggingAdapter
import utils.{IntValueClass, Base36, CompositeOrdering}

import scala.reflect.ClassTag
import scala.util.{Random, Try}
import scalaz.{-\/, \/, \/-}

/**
 * Created by arturas on 2014-09-11.
 */
package object implicits {
  implicit class OptionExts[A](val o: Option[A]) extends AnyVal {
    @inline def fold2[B](ifEmpty: => B, ifSome: A => B) = o.fold(ifEmpty)(ifSome)
  }

  implicit class TryExts[A](val t: Try[A]) extends AnyVal {
    @inline def fold[B](ifError: Throwable => B, ifSuccess: A => B) = t match {
      case util.Success(v) => ifSuccess(v)
      case util.Failure(e) => ifError(e)
    }
  }

  implicit class StringExts(val s: String) extends AnyVal {
    @inline def parseInt: Try[Int] = Try(s.toInt)
    @inline def parseDouble: Try[Double] = Try(s.toDouble)
  }

  class LeftSideException(msg: String) extends IllegalStateException(msg)

  implicit class EitherZExts[A, B](val either: A \/ B) extends AnyVal {
    def right_! = either.fold(
      err => throw new LeftSideException(s"Left where right: $err"),
      identity
    )
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
    def int(from: Int, to: Int) = from + r.nextInt(to - from)
    def range[A <: IntValueClass[A]](from: A, to: A): A =
      from.self(int(from.value, to.value))
    def double(from: Double, to: Double) = (to - from) * r.nextDouble() + from
    def chance(chance: Double) = r.nextDouble() <= chance
  }

  implicit class IndexedSeqExts[A](val is: IndexedSeq[A]) extends AnyVal {
    def random = if (is.isEmpty) None else Some(is(Random.nextInt(is.size)))
  }

  type WeightedIS[+A] = IndexedSeq[(A, Int)]

  implicit class WeightedISExts[A](val is: WeightedIS[A])
  extends AnyVal {
    /** http://stackoverflow.com/a/2149533/1513157 **/
    def weightedSample(itemCount: Int): Either[String, IndexedSeq[A]] = {
      if (is.size < itemCount)
        return s"Expected collection to have at least $itemCount items, but it had ${
          is.size} items.".left

      var sample = IndexedSeq.empty[A]
      var total = is.view.map(_._2).sum.toDouble
      var i = 0
      var (value, weight) = {
        val tuple = is(i)
        (tuple._1, tuple._2.toDouble)
      }
      while (sample.size < itemCount) {
        var x = total * (1 - math.pow(Random.nextDouble(), 1.0 / itemCount))
        total -= x
        while (x > weight) {
          x -= weight
          i += 1
          val tuple = is(i)
          value = tuple._1
          weight = tuple._2.toDouble
        }
        weight -= x
        sample :+= value
      }

      sample.right
    }

    def weightedRandom: Option[A] = weightedSample(1).right.toOption.map(_.head)
  }

  implicit class OrderingOps[T](val ord: Ordering[T]) extends AnyVal {
    def orElse(ord2: Ordering[T]) = new CompositeOrdering[T](ord, ord2)
  }

  implicit class AnyExts[A](val a: A) extends AnyVal {
    @inline def ===(a1: A) = a == a1
    @inline def =/=(a1: A) = a != a1
    @inline def |>[B](f: A => B) = f(a)
    @inline def left[B]: Either[A, B] = Left(a)
    @inline def leftZ[B]: A \/ B = -\/(a)
    @inline def right[B]: Either[B, A] = Right(a)
    @inline def rightZ[B]: B \/ A = \/-(a)
    @inline def mapVal[B](f: A => B) = f(a)
    @inline def tap(f: A => Unit) = { f(a); a }
    @inline def cast[B : ClassTag]: Option[B] =
      a match { case b: B => Some(b); case _ => None }

    @inline def some: Option[A] = Some(a)
  }

  implicit class LoggingAdapterExts(val log: LoggingAdapter) extends AnyVal {
    @inline def prefixed(prefix: String) = new PrefixedLoggingAdapter(prefix, log)
  }

  implicit class UUIDExts(val uuid: UUID) extends AnyVal {
    @inline def shortStr = s"${Base36.encode(uuid.getLeastSignificantBits)}-${
      Base36.encode(uuid.getMostSignificantBits)}"

    def toByteArray: Array[Byte] = {
      val bytes = new Array[Byte](16)
      val bb = ByteBuffer.wrap(bytes)
      bb.order(ByteOrder.BIG_ENDIAN)
      bb.putLong(uuid.getMostSignificantBits)
      bb.putLong(uuid.getLeastSignificantBits)
      bb.array()
    }
  }

  def parseUUID(arr: Array[Byte]): String \/ UUID = {
    if (arr.size != 16) s"Expected array size 16, but was ${arr.size}".leftZ
    else {
      val bb = ByteBuffer.wrap(arr)
      new UUID(bb.getLong, bb.getLong).rightZ
    }
  }
}
