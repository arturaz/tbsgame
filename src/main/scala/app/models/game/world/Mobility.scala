package app.models.game.world

/* Marker trait to disallow mixing different kinds of mobility. */
trait Mobility[A <: Mobility.Value] {
  def isMovable: Boolean
  def isStatic = ! isMovable
}
trait MobilityStatic extends Mobility[Mobility.Static.type] {
  override def isMovable = false
}
trait MobilityMovable extends Mobility[Mobility.Movable.type] {
  override def isMovable = true
}

object Mobility {
  sealed trait Value
  object Static extends Value
  object Movable extends Value
}
