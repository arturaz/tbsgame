package app.models.game.world.buildings

import akka.event.LoggingAdapter
import app.models.game.world._
import app.models.game.{Player, Actions, Population}
import implicits._

object WarpGateStats extends BuildingStats with SizedWObjectStats
with GivingActionsStats with GivingPopulationStats with SpecialActionStats {
  override val maxHp = HP(350)
  override val size = Vect2(3, 3)
  override val isCritical = true
  override val actionsGiven = Actions(3)
  override val populationGiven = Population(24)
  override val visibility = RectDistance(4)
  override val warpGiven = RectDistance(2)
  override val specialActionsNeeded = Actions(1)
  override val blocksVisibility = true
  val specialResourcesGiven = Resources(1)
  val kind = WObjKind.Heavy
}

trait WarpGateImpl extends OwnedObjImpl with SpecialActionImpl { self: WarpGate =>
  override protected def specialImpl
  (world: World, invokedBy: Player)(implicit log: LoggingAdapter) =
    world.addResources(invokedBy, stats.specialResourcesGiven)

  override def canDoSpecial(invokedBy: Player) = owner === invokedBy.team
}

case class WarpGateOps(self: WarpGate) extends OwnedObjOps[WarpGate] {
  override protected def withNewHp(hp: HP) = self.copy(hp = hp)
}