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
  /* A world update where a new World and Obj is returned. */
  type WorldObjUpdate[Obj] = Evented[(World, Obj)]
  /* A world update where a new World and Optional Obj (it might have been destroyed in a
     reaction) is returned. */
  type WorldObjOptUpdate[Obj] = WorldObjUpdate[Option[Obj]]
  @inline def newId: Id = UUID.randomUUID()
}

/* World object */
trait WObject {
  type Self = this.type
  type Companion <: WObjectOps with WObjectStats
  type WorldSelfUpdate = WObject.WorldObjUpdate[Self]

  val id: WObject.Id
  val position: Vect2

  def companion: Companion
  lazy val bounds = companion.bounds(position)

  def self: Self = this

  def gameTurnStartedSelf(world: World): WorldSelfUpdate = Evented((world, self))
  final def gameTurnStarted(world: World): Evented[World] =
    gameTurnStartedSelf(world).map(_._1)

  def gameTurnFinishedSelf(world: World): WorldSelfUpdate = Evented((world, self))
  final def gameTurnFinished(world: World): Evented[World] =
    gameTurnFinishedSelf(world).map(_._1)
  
  protected def selfUpdate
  (f: Self => Self)(evented: WorldSelfUpdate): WorldSelfUpdate =
    evented.map { case (world, self) =>
      val newSelf = f(self)
      (world.updated(self, newSelf), newSelf)
    }
}