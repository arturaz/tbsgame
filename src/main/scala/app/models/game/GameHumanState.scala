package app.models.game

import monocle.Lenser

object GameHumanState {
  private[this] val lenser = Lenser[GameHumanState]

  val actions = lenser(_.actions)
}

case class GameHumanState(actions: Actions)
