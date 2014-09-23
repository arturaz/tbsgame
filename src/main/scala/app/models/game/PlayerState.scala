package app.models.game

import monocle.Lenser

object PlayerState {
  private[this] val lenser = Lenser[PlayerState]

  val resources = lenser(_.resources)
  val actions = lenser(_.actions)
}

case class PlayerState(resources: Int, actions: Int)
