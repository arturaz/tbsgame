package app.models.world

import implicits._

trait MovableWObjectOps[Self <: WObject]
extends WObjectOps with MoveAttackActionedOps[Self]
{ _: MovableWObjectStats =>
  def setMoveValues(position: Vect2, movementLeft: TileDistance)(self: Self): Self
  def resetMovementLeft(self: Self) =
    self |> setMoveValues(self.position, movement)
}

trait MovableWObjectStats extends WObjectStats with MoveAttackActionedStats {
  val movement: TileDistance
}

trait MovableWObjectCompanion[Self <: WObject]
extends MovableWObjectOps[Self] with MovableWObjectStats

/* Objects that can move. All such objects have 1x1 size. */
trait MovableWObject extends WObject with MoveAttackActioned
with Mobility[Mobility.Movable.type] {
  type Self <: MovableWObject
  type Companion <: MovableWObjectOps[Self] with MovableWObjectStats

  val movementLeft: TileDistance
  def movementZone = movementLeft.reachable(position)

  /* Returns a set of objects which are in units movement zone. */
  def obstacles(objects: Set[WObject]) =
    // TODO: fix the optimization
    objects/*.filter(o => movementZone.exists(o.bounds.contains))*/

  override def teamTurnFinishedSelf(world: World) =
    super.teamTurnFinishedSelf(world) |>
    selfUpdate(companion.resetMovementLeft)

  def moveTo(target: Vect2): Either[String, Self] = {
    val distance = position.tileDistance(target)
    if (distance <= movementLeft)
      Right(
        self |>
          companion.setMoveValues(target, movementLeft - distance) |>
          companion.withMovedOrAttacked(true)
      )
    else
      Left(s"Logic error: can't attack target - not enough movement." +
       s" [unit:$this target:$target]")
  }
}
