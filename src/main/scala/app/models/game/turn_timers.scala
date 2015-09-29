package app.models.game

import app.models.game.TurnTimers.{TurnTime, HumanTurnTimersMap}
import app.models.game.events.{SetTurnTimerEvt, Evented}
import org.joda.time.DateTime
import utils.data.Timeframe

import scala.concurrent.duration._
import implicits._

case class TurnTimer(timeLeftPool: FiniteDuration, currentTurn: Option[Timeframe])

case class WithCurrentTime[+A](value: A, currentTime: DateTime) {
  def map[B](f: A => B) = copy(value = f(value))
  def apply[B](f: (A, DateTime) => B) = f(value, currentTime)
}

object TurnTimers {
  type HumanTurnTimersMap = Map[Human, TurnTimer]
  case class TurnTime(value: FiniteDuration) extends AnyVal
  
  case class Settings(
    maxTurnPoolSize: FiniteDuration = 35.seconds,
    /* Max turn time */
    upperTurnTimeLimit: FiniteDuration = 20.seconds,
    /* Guaranteed time given for each turn */
    constantTimeGiven: FiniteDuration = 8.seconds
  ) {
    val defaultTimer = TurnTimer(upperTurnTimeLimit, None)

    override def toString = s"TurnTimers.Settings(maxTurnPoolSize=${maxTurnPoolSize
    }, upperTurnTimeLimit=$upperTurnTimeLimit, constantTimeGiven=${constantTimeGiven
    })"
  }
  
  def apply(humans: Iterable[Human], settings: Settings): TurnTimers =
    new TurnTimers(humans.map(_ -> settings.defaultTimer)(collection.breakOut), settings)
}

class TurnTimers(
  val map: HumanTurnTimersMap, val settings: TurnTimers.Settings
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

  /* Find all humans that belong to this team, set their end turn timers to some time,
   * reduce their pools and emit events for the client. */
  def teamTurnStarted(team: Team, currentTime: DateTime): Evented[TurnTimers] = {
    val (evtNewTimers, longestTeamTurnTimeOpt) = map.keys.foldLeft(
      (Evented(this), Option.empty[FiniteDuration])
    ) {
      case ((evented, longestTeamTurnTimeOpt), human) if human.team === team =>
        val (newEvented, turnTime) = {
          val e = evented.flatMap(_.turnStartedWithTurnTime(human, currentTime))
          (e.map(_._1), e.value._2)
        }
        val newLongestTeamTurnTimeOpt =
          longestTeamTurnTimeOpt.map(_ max turnTime.value).orElse(Some(turnTime.value))
        (newEvented, newLongestTeamTurnTimeOpt)
      case (orig, _) =>
        orig
    }
    val teamTurnTimerEvt = longestTeamTurnTimeOpt.fold2(
      Vector.empty,
      longestTeamTurnTime => Vector(SetTurnTimerEvt(
        team.rightZ, Timeframe(currentTime, currentTime + longestTeamTurnTime)
      ))
    )
    evtNewTimers :++ teamTurnTimerEvt
  }

  /* Like teamTurnStarted, but for one human. */
  def turnStarted(human: Human, currentTime: DateTime): Evented[TurnTimers] =
    turnStartedWithTurnTime(human, currentTime).map(_._1)

  /* Like turnStarted, but returns the turn time for the human as well. */
  def turnStartedWithTurnTime(human: Human, currentTime: DateTime)
  : (Evented[(TurnTimers, TurnTime)]) = {
    val timer = map(human)
    val turnTime = timer.timeLeftPool min settings.upperTurnTimeLimit
    val timeframe = Timeframe(currentTime, currentTime + turnTime)
    val newTimer = TurnTimer(timer.timeLeftPool - turnTime, Some(timeframe))
    Evented(
      (withMap(map + (human -> newTimer)), TurnTime(turnTime)),
      SetTurnTimerEvt(human.leftZ, timeframe)
    )
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

  def canEqual(other: Any): Boolean = other.isInstanceOf[TurnTimers]

  override def equals(other: Any): Boolean = other match {
    case that: TurnTimers =>
      (that canEqual this) &&
        map == that.map &&
        settings == that.settings
    case _ => false
  }

  override lazy val hashCode: Int = {
    val state = Seq(map, settings)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}