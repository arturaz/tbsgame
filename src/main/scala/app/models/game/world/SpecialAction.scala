package app.models.game.world

import akka.event.LoggingAdapter
import app.models.game.{Player, Actions}
import app.models.game.events.Evented
import implicits._

import scala.language.implicitConversions
import scalaz.\/

trait SpecialActionStatsImpl { _: SpecialActionStats =>
  val specialActionsNeeded: Actions
}

trait SpecialActionImpl extends OwnedObjImpl {
  type Stats <: SpecialActionStats

  def special(world: World, invokedBy: Player)(implicit log: LoggingAdapter)
  : String \/ Evented[World] =
    if (isWarpedIn) specialImpl(world, invokedBy)
    else s"Can't do special for $this while warping in!".leftZ

  protected def specialImpl(world: World, invokedBy: Player)(implicit log: LoggingAdapter)
  : String \/ Evented[World]

  def canDoSpecial(invokedBy: Player) = owner === invokedBy
}