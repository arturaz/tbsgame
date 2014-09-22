package app.models.world

import implicits._

trait MoveAttackActionedStats extends OwnedObjStats {
  val InitialMovedOrAttacked = false
  val moveAttackActionsNeeded: Int = 1
}

trait MoveAttackActioned extends OwnedObj {
  type Self <: MoveAttackActioned
  type Stats <: MoveAttackActionedStats

  val movedOrAttacked: Boolean

  override def teamTurnFinished =
    super.teamTurnFinished |> withMovedOrAttacked(stats.InitialMovedOrAttacked)
  protected def withMovedOrAttacked(value: Boolean)(self: Self): Self
}
