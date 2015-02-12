package app.models.game

import monocle.Lenser

object GamePlayerState {
  private[this] val lenser = Lenser[GamePlayerState]

  val actions = lenser(_.actions)
}

case class GamePlayerState(actions: Actions, turnEnded: Boolean)
