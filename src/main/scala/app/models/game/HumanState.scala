package app.models.game

import monocle.Lenser

object HumanState {
  private[this] val lenser = Lenser[HumanState]

  val resources = lenser(_.resources)
  val actions = lenser(_.actions)
}

case class HumanState(resources: Int, actions: Int)
