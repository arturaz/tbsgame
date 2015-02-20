package app.models.game.world

import app.models.game.{Player, Actions}
import app.models.game.events.Evented
import implicits._

import scala.language.implicitConversions

trait SpecialActionStats extends OwnedObjStats {
  val specialActionsNeeded: Actions
}

trait SpecialActionImpl extends OwnedObjImpl {
  type Stats <: SpecialActionStats

  def special(world: World, invokedBy: Player): Either[String, Evented[World]] =
    if (isWarpedIn) specialImpl(world, invokedBy)
    else s"Can't do special for $this while warping in!".left

  protected def specialImpl(world: World, invokedBy: Player): Either[String, Evented[World]]

  def canDoSpecial(invokedBy: Player) = owner === invokedBy
}