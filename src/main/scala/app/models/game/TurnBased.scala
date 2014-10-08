package app.models.game

import app.models.game.events.Evented

/**
 * Created by arturas on 2014-09-23.
 */
trait TurnBased[A] {
  def gameTurnStarted: Evented[A]
  def gameTurnFinished: Evented[A]
  def teamTurnStarted(team: Team): Evented[A]
  def teamTurnFinished(team: Team): Evented[A]
}
