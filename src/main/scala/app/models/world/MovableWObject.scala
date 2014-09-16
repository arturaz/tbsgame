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

  protected def setMoveValues(position: Vect2, movementLeft: TileRange): Self
  def moveTo(target: Vect2): Either[String, Self] = {
    val distance = position.tileDistance(target)
    if (distance <= movementLeft.range)
      Right(setMoveValues(target, movementLeft - distance))
    else
      Left(s"Logic error: can't attack target - not enough movement." +
       s" [unit:$this target:$target]")
  }
}
