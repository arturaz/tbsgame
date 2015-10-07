package app.models.game.world

import java.util.UUID

import akka.event.LoggingAdapter
import app.models.game.events.Evented
import implicits._
import app.models.game.world.Ops._
import scalaz._, Scalaz._

import scala.reflect.ClassTag

trait WObjectStatsImpl {
  val size = Vect2.one
  def blocksVisibility = blocksWarp
  val blocksWarp = false
  val blocksMovement = true

  def bounds(position: Vect2) = Bounds(position, size)
}

trait WObjectImpl {
  val id: WObject.Id
  val position: Vect2

  type Stats <: WObjectStats
  def stats: Stats

  // Needs to be lazy, because stats might get overriden
  def bounds = stats.bounds(position)
}

trait WObjectCompanion {
  type Static = WObject with Mobility[Mobility.Static.type]
  object Static {
    def unapply(o: WObject) = o match {
      case static: WObject with Mobility[_] if static.isStatic =>
        Some(static.asInstanceOf[Static])
      case _ => None
    }
  }

  /* A world update where a new World and Obj is returned. */
  type WorldObjUpdate[+Obj] = Evented[(World, Obj)]
  /* A world update where a new World and Optional Obj (it might have been destroyed in a
     reaction) is returned. */
  type WorldObjOptUpdate[+Obj] = WorldObjUpdate[Option[Obj]]
  @inline def newId = WObject.Id(UUID.randomUUID())

  final def roundStarted
  (world: World, obj: WObject)(implicit log: LoggingAdapter)
  : Evented[(World, Option[WObject])] = {
    Evented((world, obj)) |>
      ifIs[TurnCounter].evt((w, o) => o.roundStarted(w)) |>
      ifIs[Warpable].evt((w, o) => o.warpableTeamTurnStarted(w)) |>
      ifIs[GivingVictoryPoints].rawWorld((w, o) => o.givingVPsTeamTurnStarted(w)) |>
      ifIs[Extractor].evtWorld((w, o) => o.extractorTeamTurnStarted(w)) |>
      ifIs[Movable].evt((w, o) => o.movableTeamTurnStarted(w)) |>
      ifIs[Fighter].evt((w, o) => o.fighterTeamTurnStarted(w)) |>
      (_.map { case (newWorld, newObj) => (newWorld, Some(newObj)) })
  }

  final def roundEnded
  (world: World, obj: WObject)(implicit log: LoggingAdapter)
  : Evented[(World, Option[WObject])] = {
    Evented((world, obj)) |>
      ifIs[ReactiveFighter].evtOpt((w, o) => o.reactiveFighterTeamTurnFinished(w))
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

  class IfIs[CastClass <: WObject : ClassTag]() {
    def evt[Base >: CastClass]
    (f: (World, CastClass) => Evented[(World, CastClass)])
    (evt: Evented[(World, Base)]): Evented[(World, Base)] =
      evt.flatMap { case orig @ (w, o) =>
        o.cast[CastClass].fold2(Evented(orig), f(w, _))
      }

    def evtOpt[Base >: CastClass]
    (f: (World, CastClass) => Evented[(World, Option[CastClass])])
    (evt: Evented[(World, Base)]): Evented[(World, Option[Base])] =
      evt.flatMap { case (w, o) =>
        o.cast[CastClass].fold2(Evented((w, Some(o))), f(w, _))
      }

    def evtWorld[Base >: CastClass]
    (f: (World, CastClass) => Evented[World])
    (evt: Evented[(World, Base)]) =
      this.evt[Base]((w, o) => f(w, o).map((_, o)))(evt)

    def raw[Base >: CastClass]
    (f: (World, CastClass) => (World, CastClass))(evt: Evented[(World, Base)]) =
      this.evt[Base]((w, o) => Evented(f(w, o)))(evt)

    def rawWorld[Base >: CastClass]
    (f: (World, CastClass) => World)(evt: Evented[(World, Base)]) =
      this.raw[Base]((w, o) => (f(w, o), o))(evt)
  }

  def ifIs[CastClass <: WObject : ClassTag] = new IfIs[CastClass]
}