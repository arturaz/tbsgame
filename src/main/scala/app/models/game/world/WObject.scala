package app.models.game.world

import java.util.UUID

import app.models.game.events.Evented
import utils.IdObj
import implicits._

trait WObjectStats

object WObject {
  case class Id(id: UUID) extends AnyVal with IdObj {
    override protected def prefix = "WObjID"
  }
  /* A world update where a new World and Obj is returned. */
  type WorldObjUpdate[+Obj] = Evented[(World, Obj)]
  /* A world update where a new World and Optional Obj (it might have been destroyed in a
     reaction) is returned. */
  type WorldObjOptUpdate[+Obj] = WorldObjUpdate[Option[Obj]]
  @inline def newId: Id = Id(UUID.randomUUID())

  def bounds(position: Vect2) = Bounds(position, Vect2.one)

//  final def gameTurnStarted(world: World)(implicit log: LoggingAdapter): Evented[World] =
//    gameTurnStartedSelf(world).map(_._1)
//
//  def gameTurnStarted[Self <: WObject]
//  (world: World)(obj: Self)(implicit log: LoggingAdapter): Evented[(World, Self)] =
//    ???
    //Evented((world, self))

//  type WorldSelfUpdate = WObject.WorldObjUpdate[Self]
//
//  def gameTurnStartedSelf(world: World)(implicit log: LoggingAdapter): WorldSelfUpdate =
//    Evented((world, self))
//  final def gameTurnStarted(world: World)(implicit log: LoggingAdapter): Evented[World] =
//    gameTurnStartedSelf(world).map(_._1)
//
//  def gameTurnFinishedSelf(world: World)(implicit log: LoggingAdapter): WorldSelfUpdate =
//    Evented((world, self))
//  final def gameTurnFinished(world: World)(implicit log: LoggingAdapter): Evented[World] =
//    gameTurnFinishedSelf(world).map(_._1)
//
  def selfEventedUpdate[Self <: WObject]
  (f: (World, Self) => Evented[Self])
  (evented: WorldObjUpdate[Self])
  : WorldObjUpdate[Self] = {
    val evt = evented.flatMap { case (world, self) =>
      f(world, self).flatMap { newSelf =>
        world.updated(self, newSelf).map((_, newSelf))
      }
    }
    evt.value._2.cast[OwnedObj].fold2(
      evt,
      self => World.revealObjects(self.owner.team, evt.map(_._1)).map((_, evt.value._2))
    )
  }

  def selfEventedUpdate[Self <: WObject]
  (world: World, self: Self, newEvtSelf: Evented[Self]): Evented[(World, Self)] = {
    val evt = newEvtSelf.flatMap { newSelf =>
      world.updated(self, newSelf).map((_, newSelf))
    }
    evt.value._2.cast[OwnedObj].fold2(
      evt,
      self => World.revealObjects(self.owner.team, evt.map(_._1)).map((_, evt.value._2))
    )
  }

//
//  protected def selfUpdate
//  (f: Self => Self)(evented: WorldSelfUpdate): WorldSelfUpdate =
//    selfEventedUpdate((_, self) => Evented(f(self)))(evented)
}