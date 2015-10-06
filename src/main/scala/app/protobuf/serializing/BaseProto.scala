package app.protobuf.serializing

import java.util.UUID

import app.models.game.world.{Bounds, Vect2}
import org.joda.time.DateTime
import spire.math.{UInt, Rational}
import utils.data.Timeframe
import utils.{RationalValueClass, ValWithMax, IntValueClass}
import netmsg.{base => nbase}

import language.implicitConversions

trait BaseProto extends Helpers {
  def valWithMax(current: Int, maximum: Int): nbase.ValWithMax =
    nbase.ValWithMax(current = current, maximum = maximum)

  implicit def convert[A <: IntValueClass[A]](vwm: ValWithMax[A]): nbase.ValWithMax =
    nbase.ValWithMax(current = vwm.value, maximum = vwm.max)

  implicit def convert(uint: UInt): Int = uint.signed

  implicit def convert(dateTime: DateTime): nbase.Timestamp =
    nbase.Timestamp(dateTime.getMillis)

  implicit def convert(tf: Timeframe): nbase.Timeframe =
    nbase.Timeframe(start = tf.start, end = tf.end)

  implicit def convert(id: UUID): nbase.UUID = nbase.UUID(
    leastSignificant = id.getLeastSignificantBits,
    mostSignificant = id.getMostSignificantBits
  )

  implicit def convert(v: RationalValueClass[_]): nbase.Rational = v.value
  
  implicit def convert(r: Rational): nbase.Rational =
    nbase.Rational(numerator = r.numeratorAsLong, denominator = r.denominatorAsLong)

  implicit def convert(range: Range.Inclusive): nbase.Range =
    nbase.Range(start = range.start, end = range.end)

  implicit def convert(v: Vect2): nbase.Vect2 =
    netmsg.base.Vect2(x = v.x, y = v.y)

  implicit def convert(b: Bounds): nbase.Bounds =
    nbase.Bounds(start = b.start, end = b.end)
}
