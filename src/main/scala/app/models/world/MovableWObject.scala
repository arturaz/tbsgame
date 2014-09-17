package app.models.world

trait MovableWObjectStats extends WObjectStats {
  val movement: TileRange
}

/* Objects that can move. All such objects have 1x1 size. */
trait MovableWObject extends WObject {
  type Self <: MovableWObject
  type Stats <: MovableWObjectStats

  val movementLeft: TileRange
  def movementZone = movementLeft.reachable(position)

  def obstacles(objects: Set[WObject]) =
    objects.filter(o => movementZone.exists(o.bounds.contains))

  override def nextTurn = setMoveValues(super.nextTurn, position, stats.movement)

  protected def setMoveValues(self: Self, position: Vect2, movementLeft: TileRange): Self
  def moveTo(target: Vect2): Either[String, Self] = {
    val distance = position.tileDistance(target)
    if (distance <= movementLeft.range)
      Right(setMoveValues(self, target, movementLeft - distance))
    else
      Left(s"Logic error: can't attack target - not enough movement." +
       s" [unit:$this target:$target]")
  }
}
