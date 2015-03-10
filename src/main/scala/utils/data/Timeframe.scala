package utils.data

import org.joda.time.DateTime

import scala.concurrent.duration.FiniteDuration
import implicits._

case class Timeframe(start: DateTime, end: DateTime) extends Ordered[Timeframe] {
  def timeUsedUp(currentTime: DateTime): Boolean = currentTime >= end
  def duration: FiniteDuration = end - start

  override def compare(that: Timeframe) = duration compare that.duration
}