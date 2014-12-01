package app.models.game

import akka.event.LoggingAdapter
import app.models.game.events.Evented

/**
 * Created by arturas on 2014-09-23.
 */
trait TurnBased[A] {
  def gameTurnStarted(implicit log: LoggingAdapter): Evented[A]
  def gameTurnFinished(implicit log: LoggingAdapter): Evented[A]
  def teamTurnStarted(team: Team)(implicit log: LoggingAdapter): Evented[A]
  def teamTurnFinished(team: Team)(implicit log: LoggingAdapter): Evented[A]
}
