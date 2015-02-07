package app.models.game.world

import java.util.UUID

import akka.event.LoggingAdapter
import app.models.game.events.Evented
import utils.IdObj
import implicits._

import scala.reflect.ClassTag

trait WObjectStats

trait WObjectImpl {
  val id: WObject.Id
  val position: Vect2
  val stats: WObjectStats

  lazy val bounds = WObject.bounds(position)
}

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

  final def gameTurnStarted
  (world: World, obj: WObject)(implicit log: LoggingAdapter)
  : Evented[(World, WObject)] = {
    import TurnCounter.toTurnCounterOps

    Evented((world, obj)) |>
      ifIs[TurnCounter].evt((w, o) => o.gameTurnStarted(w))
  }

  final def gameTurnFinished
  (world: World, obj: WObject)(implicit log: LoggingAdapter)
  : Evented[(World, WObject)] = {
    Evented((world, obj))
  }

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

  class IfIs[To : ClassTag]() {
    def evt[A <: WObject]
    (f: (World, To) => Evented[(World, A)])
    (evt: Evented[(World, A)]) =
      evt.flatMap { case orig @ (w, o) =>
        o.cast[To].fold2(Evented(orig), f(w, _))
      }

    def raw[A <: WObject]
    (f: (World, To) => (World, A))(evt: Evented[(World, A)]) =
      this.evt[A]((w, o) => Evented(f(w, o)))(evt)
  }

  def ifIs[To : ClassTag] = new IfIs[To]
}