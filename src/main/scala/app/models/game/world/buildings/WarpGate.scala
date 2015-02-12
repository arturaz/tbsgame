package app.models.game.world.buildings

import app.models.game.world._
import app.models.game.{Actions, Population}

object WarpGateStats extends BuildingStats with SizedWObjectStats
with GivingActionsStats with GivingPopulationStats {
  override val maxHp = HP(800)
  override val size = Vect2(6, 3)
  override val isCritical: Boolean = true
  override val actionsGiven = Actions(3)
  override val populationGiven = Population(24)
  override val visibility = RectDistance(4)
  override val warpGiven = RectDistance(2)
  val kind = WObjKind.Heavy
}

case class WarpGateOps(self: WarpGate) extends OwnedObjOps[WarpGate] {
  override protected def withNewHp(hp: HP) = self.copy(hp = hp)
}