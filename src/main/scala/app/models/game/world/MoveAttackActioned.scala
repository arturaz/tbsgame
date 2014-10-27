package app.models.game.world

import app.models.game.Actions
import app.models.game.events.{MovedOrAttackedChangeEvt, Evented}
import implicits._

trait MoveAttackActionedOps[Self <: MoveAttackActioned] extends OwnedObjOps[Self] {
  protected def withMovedOrAttacked(value: Boolean)(self: Self): Self
  def withMovedOrAttackedEvt(world: World, value: Boolean)(self: Self): Evented[Self] = {
    for {
      newSelf <- Evented(self |> withMovedOrAttacked(value))
      newSelf <- Evented(
        newSelf,
        if (self.movedOrAttacked =/= newSelf.movedOrAttacked)
          Vector(MovedOrAttackedChangeEvt(world, newSelf))
        else Vector.empty
      )
    } yield newSelf
  }
}

trait MoveAttackActionedStats extends OwnedObjStats {
  val InitialMovedOrAttacked = false
  val moveAttackActionsNeeded: Actions = Actions(1)
}

trait MoveAttackActionedCompanion[Self <: MoveAttackActioned]
extends MoveAttackActionedOps[Self] with MoveAttackActionedStats

trait MoveAttackActioned extends OwnedObj {
  type Self <: MoveAttackActioned
  type Companion <: MoveAttackActionedOps[Self] with MoveAttackActionedStats

  val movedOrAttacked: Boolean
}
