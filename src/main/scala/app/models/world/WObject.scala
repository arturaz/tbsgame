package app.models.world

import java.util.UUID

trait WObjectStats

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
  lazy val bounds = Bounds(position, Vect2.one)

  protected def self: Self
  def nextTurn = self
}
