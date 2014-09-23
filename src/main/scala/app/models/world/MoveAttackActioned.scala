package app.models.world

import implicits._

trait MoveAttackActionedOps[Self] extends OwnedObjOps[Self] {
  def withMovedOrAttacked(value: Boolean)(self: Self): Self
}

trait MoveAttackActionedStats extends OwnedObjStats {
  val InitialMovedOrAttacked = false
  val moveAttackActionsNeeded: Int = 1
}

trait MoveAttackActionedCompanion[Self]
extends MoveAttackActionedOps[Self] with MoveAttackActionedStats

trait MoveAttackActioned extends OwnedObj {
  type Companion <: MoveAttackActionedOps[Self] with MoveAttackActionedStats

  val movedOrAttacked: Boolean

  override def teamTurnFinishedSelf(world: World) =
    super.teamTurnFinishedSelf(world) |>
    selfUpdate(
      _ |> companion.withMovedOrAttacked(companion.InitialMovedOrAttacked)
    )
}
