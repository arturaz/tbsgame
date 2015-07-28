package app.models.game.world.buildings

import app.models.game.{Player, Population}
import app.models.game.world._

trait PopulationTowerStatsImpl extends EmptySpaceWarpableCompanion[PopulationTower] {
_: PopulationTowerStats.type =>
  override val populationGiven = Population(5)
  override val kind = WObjKind.Light
  override val maxHp = HP(250)
  override val warpTime = WarpTime(2)
  override val cost = Resources(9)
  override val group = WarpableGroup.Building
  override val populationCost = Population(0)
  override val size = Vect2(2, 2)

  override def warp(owner: Player, position: Vect2) = PopulationTower(position, owner)
}

trait PopulationTowerImpl extends WarpableImpl with BuildingImpl { _: PopulationTower =>
}

case class PopulationTowerOps(self: PopulationTower)
extends OwnedObjOps[PopulationTower] with WarpableOps[PopulationTower] {
  override protected def withNewHp(hp: HP) = self.copy(hp = hp)
  override def setWarpState(newState: WarpTime) = self.copy(warpState = newState)
}