package app.models.game

import app.models.game.TurnTimers.{TurnTime, HumanTurnTimersMap}
import org.joda.time.DateTime
import utils.data.Timeframe

import scala.concurrent.duration._
import implicits._
import scalaz._, Scalaz._

case class TurnTimer(timeLeftPool: FiniteDuration, currentTurn: Option[Timeframe])

case class WithCurrentTime[+A](value: A, currentTime: DateTime) {
  def map[B](f: A => B) = copy(value = f(value))
  def apply[B](f: (A, DateTime) => B) = f(value, currentTime)
}

object TurnTimers {
  type HumanTurnTimersMap = Map[Human, TurnTimer]
  case class TurnTime(value: FiniteDuration) extends AnyVal
  
  case class Settings(
    maxTurnPoolSize: FiniteDuration = 60.seconds,
    /* Max turn time */
    upperTurnTimeLimit: FiniteDuration = 36.seconds,
    /* Guaranteed time given for each turn */
    constantTimeGiven: FiniteDuration = 20.seconds
  ) {
    val defaultTimer = TurnTimer(upperTurnTimeLimit, None)

    override def toString = s"TurnTimers.Settings(maxTurnPoolSize=${maxTurnPoolSize
    }, upperTurnTimeLimit=$upperTurnTimeLimit, constantTimeGiven=${constantTimeGiven
    })"
  }
  
  def apply(humans: Iterable[Human], settings: Settings): TurnTimers =
    new TurnTimers(humans.map(_ -> settings.defaultTimer)(collection.breakOut), settings)
}

case class TurnTimers(
  map: HumanTurnTimersMap, settings: TurnTimers.Settings
) {
  private[this] def withMap(map: HumanTurnTimersMap) = new TurnTimers(map, settings)

  /* Find given human, set their end turn timer to none and add the remaining time to
   * their pool */
  def endTurn(human: Human, currentTime: DateTime): TurnTimers = {
    map.get(human).collect { case TurnTimer(poolTime, Some(timeframe)) =>
      val remainingTime = (timeframe.end - currentTime) max 0.seconds
      TurnTimer(
        (poolTime + remainingTime + settings.constantTimeGiven)
          min settings.maxTurnPoolSize, None
      )
    }.map(timer => withMap(map + (human -> timer))).getOrElse(this)
  }

  /* Like teamTurnStarted, but for one human. */
  def turnStarted(human: Human, currentTime: DateTime): TurnTimers =
    turnStartedWithTurnTime(human, currentTime)._1

  /* Like turnStarted, but returns the turn time for the human as well. */
  def turnStartedWithTurnTime(human: Human, currentTime: DateTime)
  : (TurnTimers, TurnTime) = {
    val timer = map(human)
    val turnTime = timer.timeLeftPool min settings.upperTurnTimeLimit
    val timeframe = Timeframe(currentTime, currentTime + turnTime)
    val newTimer = TurnTimer(timer.timeLeftPool - turnTime, Some(timeframe))
    (withMap(map + (human -> newTimer)), TurnTime(turnTime))
  }

  def maxTimeframeFor(team: Team): Option[Timeframe] =
    map.view.filter(_._1.team === team).foldLeft(Option.empty[Timeframe]) {
      case (None, (human, TurnTimer(_, timeframeOpt))) =>
        timeframeOpt
      case (Some(current), (human, TurnTimer(_, Some(timeframe)))) =>
        Some(current max timeframe)
      case (current, _) =>
        current
    }

  override def toString = s"TurnTimers($settings, timers=$map)"
}
