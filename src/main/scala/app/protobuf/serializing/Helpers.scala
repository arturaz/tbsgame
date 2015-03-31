package app.protobuf.serializing

import utils.{FloatValueClass, DoubleValueClass, IntValueClass}

import language.implicitConversions

trait Helpers {
  implicit def convert(v: IntValueClass[_]): Int = v.value
  implicit def convert(v: DoubleValueClass[_]): Double = v.value
  implicit def convert(v: FloatValueClass[_]): Float = v.value

  def convertSeq[A, B](seq: Iterable[A])(implicit converter: A => B): Seq[B] =
    seq.map(converter)(collection.breakOut)
}
