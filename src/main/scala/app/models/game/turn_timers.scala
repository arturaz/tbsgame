package app.models.game

import app.models.game.TurnTimers.HumanTurnTimersMap
import app.models.game.events.{SetTurnTimerEvt, Evented}
import org.joda.time.DateTime

import scala.concurrent.duration._
import implicits._

case class TurnTimeframe(start: DateTime, end: DateTime) {
  def timeUsedUp(currentTime: DateTime): Boolean = currentTime >= end
}

case class TurnTimer(timeLeftPool: FiniteDuration, currentTurn: Option[TurnTimeframe])

object TurnTimers {
  type HumanTurnTimersMap = Map[Human, TurnTimer]
  
  case class Settings(
    maxTurnPoolSize: FiniteDuration = 5.minutes,
    /* Max turn time */
    upperTurnTimeLimit: FiniteDuration = 2.minutes,
    /* Guaranteed time given for each turn */
    constantTimeGiven: FiniteDuration = 1.minute
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
    val (evtNewTimers, longestTeamTurnTimeOpt) = map.foldLeft(
      (Evented(Map.empty: HumanTurnTimersMap), Option.empty[FiniteDuration])
    ) {
      case ((evented, longestTeamTurnTimeOpt), (human, timer)) if human.team === team =>
        val turnTime = timer.timeLeftPool min settings.upperTurnTimeLimit
        val timeframe = TurnTimeframe(currentTime, currentTime + turnTime)
        val newTimer = TurnTimer(timer.timeLeftPool - turnTime, Some(timeframe))
        val newLongestTeamTurnTimeOpt =
          longestTeamTurnTimeOpt.map(_ max turnTime).orElse(Some(turnTime))
        val newEvented = evented.flatMap { map => Evented(
          map + (human -> newTimer),
          // Turn timer for each individual human.
          SetTurnTimerEvt(human.leftZ, timeframe)
        ) }
        (newEvented, newLongestTeamTurnTimeOpt)
      case ((evtMap, longestTeamTurnTimeOpt), tuple) =>
        (evtMap.map(_ + tuple), longestTeamTurnTimeOpt)
    }
    val teamTurnTimerEvt = longestTeamTurnTimeOpt.fold2(
      Vector.empty,
      longestTeamTurnTime => Vector(SetTurnTimerEvt(
        team.rightZ, TurnTimeframe(currentTime, currentTime + longestTeamTurnTime)
      ))
    )
    evtNewTimers.map(withMap) :++ teamTurnTimerEvt
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