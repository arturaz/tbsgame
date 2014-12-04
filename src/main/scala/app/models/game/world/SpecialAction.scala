package app.models.game.world

import app.models.game.Actions
import app.models.game.events.Evented
import implicits._

trait SpecialActionOps[Self <: SpecialAction] extends OwnedObjOps[Self]

trait SpecialActionStats extends OwnedObjStats {
  val specialActionsNeeded: Actions
}

trait SpecialActionCompanion[Self <: SpecialAction] extends SpecialActionOps[Self]
with SpecialActionStats

trait SpecialAction extends OwnedObj {
  type Self <: SpecialAction
  type Companion <: SpecialActionOps[Self] with SpecialActionStats

  def special(world: World): Either[String, Evented[World]] =
    if (isWarpedIn) specialImpl(world)
    else s"Can't do special for $self while warping in!".left

  protected def specialImpl(world: World): Either[String, Evented[World]]
}
