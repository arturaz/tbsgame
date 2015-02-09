package app.models.game.world

import app.models.game.Actions
import app.models.game.events.Evented
import implicits._

import scala.language.implicitConversions

trait SpecialActionStats extends OwnedObjStats {
  val specialActionsNeeded: Actions
}

trait SpecialActionImpl extends OwnedObjImpl {
  type Stats <: SpecialActionStats

  def special(world: World): Either[String, Evented[World]] =
    if (isWarpedIn) specialImpl(world)
    else s"Can't do special for $this while warping in!".left

  protected def specialImpl(world: World): Either[String, Evented[World]]
}