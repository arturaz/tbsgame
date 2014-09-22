package app.models.world

import java.util.UUID

trait WObjectStats {
  def bounds(position: Vect2) = Bounds(position, Vect2.one)
}

object WObject {
  type Id = UUID
  def newId: Id = UUID.randomUUID()
}

/* World object */
trait WObject {
  type Self <: WObject
  type Stats <: WObjectStats

  val id: WObject.Id
  val position: Vect2

  def stats: Stats
  lazy val bounds = stats.bounds(position)

  protected def self: Self
  /* Called when game turn is finished to get new copy of self. */
  def gameTurnFinished = self
}