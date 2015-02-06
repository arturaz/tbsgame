package app.models.game.world

import akka.event.LoggingAdapter
import app.models.game.events.{Evented, HPChangeEvt}
import app.models.game.world.buildings.Extractor
import implicits._

import scala.language.implicitConversions

trait OwnedObjStats extends WObjectStats {
  val maxHp: HP
  val visibility: RectDistance
  /* If team has no critical objects it cannot do any more actions and
     loses the game. */
  val isCritical: Boolean = false
  val warpGiven: RectDistance = RectDistance(0)
  val kind: WObjKind
}

object OwnedObj extends ToOwnedObjOps {
  def teamTurnStarted
  (obj: OwnedObj, world: World)(implicit log: LoggingAdapter)
  : Evented[(OwnedObj, World)] = {
    import GivingVictoryPoints.toGivingVictoryPointsOps
    val empty = Evented((world, obj))

    for {
      (world, obj) <- empty
      (world, obj) <-
        obj.cast[GivingVictoryPoints].fold2(empty, x => (x.teamTurnStarted(world), x))
    } yield (obj, world)
  }

  def teamTurnFinished
  (obj: OwnedObj, world: World)(implicit log: LoggingAdapter)
  : Evented[(OwnedObj, World)] = {
    import app.models.game.world.Movable.toMovableOps
    val empty = Evented((world, obj))

    for {
      (world, obj) <- empty
      (world, obj) <- obj.cast[Movable].fold2(empty, x => x.teamTurnFinishedSelf(world))
    } yield (obj, world)
  }
}

trait OwnedObjOps[Self <: OwnedObj] {
  def self: Self
  def withNewHp(hp: HP): Self
  def withNewHPEvt(hp: HP)(world: World): Evented[Self] = {
    val newSelf = withNewHp(hp)
    Evented(
      newSelf,
      if (self.hp == newSelf.hp) Vector.empty else Vector(HPChangeEvt(world, newSelf))
    )
  }

  def takeDamage(damage: HP): Option[Self] =
    if (self.hp <= damage) None else Some(withNewHp(self.hp - damage))
}

trait ToOwnedObjOps {
  implicit def toOwnedObjOps[A <: OwnedObj](a: A): OwnedObjOps[A] = (a match {
    case o: Extractor => Extractor.Ops(o)
  }).asInstanceOf[OwnedObjOps[A]]
}
