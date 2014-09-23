package app.models.world

import java.util.UUID

import app.models.game.events.Evented

trait WObjectOps {
  def bounds(position: Vect2) = Bounds(position, Vect2.one)
}

trait WObjectStats

trait WObjectCompanion extends WObjectOps with WObjectStats

object WObject {
  type Id = UUID
  @inline def newId: Id = UUID.randomUUID()
}

/* World object */
trait WObject {
  type Self <: WObject
  type Companion <: WObjectOps with WObjectStats

  val id: WObject.Id
  val position: Vect2

  def companion: Companion
  lazy val bounds = companion.bounds(position)

  protected def self: Self

  def gameTurnStartedSelf(world: World): Evented[(World, Self)] =
    Evented((world, self))
  final def gameTurnStarted(world: World): Evented[World] =
    gameTurnStartedSelf(world).map(_._1)

  def gameTurnFinishedSelf(world: World): Evented[(World, Self)] =
    Evented((world, self))
  final def gameTurnFinished(world: World): Evented[World] =
    gameTurnFinishedSelf(world).map(_._1)
  
  protected def selfUpdate
  (f: Self => Self)(evented: Evented[(World, Self)]): Evented[(World, Self)] =
    evented.map { case (world, self) =>
      val newSelf = f(self)
      (world.updated(self, newSelf), newSelf)
    }
}