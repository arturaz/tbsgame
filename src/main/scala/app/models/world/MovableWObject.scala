package app.models.world

trait MovableWObjectStats extends WObjectStats {
  val movement: TileDistance
}

/* Objects that can move. All such objects have 1x1 size. */
trait MovableWObject extends WObject with Mobility[Mobility.Movable.type] {
  type Self <: MovableWObject
  type Stats <: MovableWObjectStats

  val movementLeft: TileDistance
  def movementZone = movementLeft.reachable(position)

  /* Returns a set of objects which are in units movement zone. */
  def obstacles(objects: Set[WObject]) =
    // TODO: fix the optimization
    objects/*.filter(o => movementZone.exists(o.bounds.contains))*/

  override def nextTurn = setMoveValues(super.nextTurn, position, stats.movement)

  protected def setMoveValues(self: Self, position: Vect2, movementLeft: TileDistance): Self
  def moveTo(target: Vect2): Either[String, Self] = {
    val distance = position.tileDistance(target)
    if (distance <= movementLeft)
      Right(setMoveValues(self, target, movementLeft - distance))
    else
      Left(s"Logic error: can't attack target - not enough movement." +
       s" [unit:$this target:$target]")
  }
}
