package app.models.game

import monocle.Lenser

object PlayerState {
  case class Actions(total: Int, current: Int)

  object Actions {
    def apply(total: Int): Actions = Actions(total, total)

    private[this] val lenser = Lenser[Actions]

    val total = lenser(_.total)
    val current = lenser(_.current)
  }

  private[this] val lenser = Lenser[PlayerState]

  val resources = lenser(_.resources)
  val actions = lenser(_.actions)
}

case class PlayerState(
  resources: Int, actions: PlayerState.Actions
)
