package app.models.game.world

import java.util.UUID

import akka.event.LoggingAdapter
import app.models.game.events.Evented
import implicits._
import app.models.game.world.Ops._

import scala.reflect.ClassTag

trait WObjectStats {
  val size = Vect2.one
  val blocksVisibility = false
  val blocksWarp = false

  def bounds(position: Vect2) = Bounds(position, size)
}

trait WObjectImpl {
  val id: WObject.Id
  val position: Vect2

  type Stats <: WObjectStats
  val stats: Stats

  // Needs to be lazy, because stats might get overriden
  def bounds = stats.bounds(position)
}

trait WObjectCompanion {
  /* A world update where a new World and Obj is returned. */
  type WorldObjUpdate[+Obj] = Evented[(World, Obj)]
  /* A world update where a new World and Optional Obj (it might have been destroyed in a
     reaction) is returned. */
  type WorldObjOptUpdate[+Obj] = WorldObjUpdate[Option[Obj]]
  @inline def newId = WObject.Id(UUID.randomUUID())

  final def gameTurnStarted
  (world: World, obj: WObject)(implicit log: LoggingAdapter)
  : Evented[(World, WObject)] = {
    Evented((world, obj)) |>
      ifIs[TurnCounter].evt((w, o) => o.gameTurnStarted(w))
  }

  final def gameTurnFinished
  (world: World, obj: WObject)(implicit log: LoggingAdapter)
  : Evented[(World, WObject)] = {
    Evented((world, obj))
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

  class IfIs[To <: WObject : ClassTag]() {
    def evt[A >: To]
    (f: (World, To) => Evented[(World, To)])
    (evt: Evented[(World, A)]) =
      evt.flatMap { case orig @ (w, o) =>
        o.cast[To].fold2(Evented(orig), f(w, _))
      }

    def evtWorld[A >: To]
    (f: (World, To) => Evented[World])
    (evt: Evented[(World, A)]) =
      this.evt[A]((w, o) => f(w, o).map((_, o)))(evt)

    def raw[A >: To]
    (f: (World, To) => (World, To))(evt: Evented[(World, A)]) =
      this.evt[A]((w, o) => Evented(f(w, o)))(evt)

    def rawWorld[A >: To]
    (f: (World, To) => World)(evt: Evented[(World, A)]) =
      this.raw[A]((w, o) => (f(w, o), o))(evt)
  }

  def ifIs[To <: WObject : ClassTag] = new IfIs[To]
}