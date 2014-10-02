package app.models.game

import monocle.Lenser

object HumanState {
  private[this] val lenser = Lenser[HumanState]

  val actions = lenser(_.actions)
}

case class HumanState(actions: Int)
