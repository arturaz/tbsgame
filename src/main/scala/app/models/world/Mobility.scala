package app.models.world

/* Marker trait to disallow mixing different kinds of mobility. */
trait Mobility[A <: Mobility.Value]

object Mobility {
  sealed trait Value
  object Static extends Value
  object Movable extends Value
}
