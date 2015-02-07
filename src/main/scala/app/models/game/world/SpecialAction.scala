package app.models.game.world

import app.models.game.Actions
import app.models.game.events.Evented
import implicits._

import scala.language.implicitConversions

trait SpecialActionStats extends OwnedObjStats {
  val specialActionsNeeded: Actions
}

trait SpecialActionImpl extends OwnedObjImpl {
  val stats: SpecialActionStats
}

trait SpecialActionOps[Self <: SpecialAction] {
  def self: Self

  def special(world: World): Either[String, Evented[World]] =
    if (self.isWarpedIn) specialImpl(world)
    else s"Can't do special for $self while warping in!".left

  protected def specialImpl(world: World): Either[String, Evented[World]]
}

trait ToSpecialActionOps {
  implicit def toSpecialActionOps[A <: SpecialAction]
  (a: A): SpecialActionOps[A] = (a match {

  }).asInstanceOf[SpecialActionOps[A]]
}

object SpecialAction extends ToSpecialActionOps