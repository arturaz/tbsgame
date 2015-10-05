package app.models.game.world

import akka.event.LoggingAdapter
import app.models.game.Player
import implicits._
import scalaz._, Scalaz._

trait SpecialActionGetResourcesStatsImpl { _: SpecialActionGetResourcesStats =>
  def specialResourcesGiven: Resources
}

trait SpecialActionGetResourcesImpl extends SpecialActionImpl {
_: SpecialActionGetResources =>
  type Stats <: SpecialActionGetResourcesStats

  override protected def specialImpl
  (world: World, invokedBy: Player)(implicit log: LoggingAdapter) =
    world.addResources(invokedBy, stats.specialResourcesGiven)

  override def canDoSpecial(invokedBy: Player) = owner === invokedBy.team
}
