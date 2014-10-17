package app.models.game.world

import app.models.game.Actions

trait MoveAttackActionedOps[Self] extends OwnedObjOps[Self] {
  def withMovedOrAttacked(value: Boolean)(self: Self): Self
}

trait MoveAttackActionedStats extends OwnedObjStats {
  val InitialMovedOrAttacked = false
  val moveAttackActionsNeeded: Actions = Actions(1)
}

trait MoveAttackActionedCompanion[Self]
extends MoveAttackActionedOps[Self] with MoveAttackActionedStats

trait MoveAttackActioned extends OwnedObj {
  type Companion <: MoveAttackActionedOps[Self] with MoveAttackActionedStats

  val movedOrAttacked: Boolean
}
