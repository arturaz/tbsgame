package app.models.world

import implicits._

trait MovableWObjectStats extends WObjectStats with MoveAttackActionedStats {
  val movement: TileDistance
}

/* Objects that can move. All such objects have 1x1 size. */
trait MovableWObject extends WObject with MoveAttackActioned
with Mobility[Mobility.Movable.type] {
  type Self <: MovableWObject
  type Stats <: MovableWObjectStats

  val movementLeft: TileDistance
  def movementZone = movementLeft.reachable(position)

  /* Returns a set of objects which are in units movement zone. */
  def obstacles(objects: Set[WObject]) =
    // TODO: fix the optimization
    objects/*.filter(o => movementZone.exists(o.bounds.contains))*/

  override def teamTurnFinished = super.teamTurnFinished |> setMoveValues(position, stats.movement)

  protected def setMoveValues(position: Vect2, movementLeft: TileDistance)(self: Self): Self

  def moveTo(target: Vect2): Either[String, Self] = {
    val distance = position.tileDistance(target)
    if (distance <= movementLeft)
      Right(
        self |>
          setMoveValues(target, movementLeft - distance) |>
          withMovedOrAttacked(true)
      )
    else
      Left(s"Logic error: can't attack target - not enough movement." +
       s" [unit:$this target:$target]")
  }
}
