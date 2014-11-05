package app.models.game.world.buildings

import app.models.game.world._

trait BuildingOps[Self] extends OwnedObjOps[Self] {
}

trait BuildingStats extends OwnedObjStats {
  override val visibility = 3
  override val kind = WObjKind.Heavy
}

trait BuildingCompanion[Self] extends BuildingOps[Self] with BuildingStats

/* Building existing in game world. */
trait Building extends OwnedObj {
  type Companion <: BuildingOps[Self] with BuildingStats
}

trait PlayerBuilding extends Building with PlayerObj
trait HumanBuilding extends Building with HumanObj
trait BotBuilding extends Building with BotObj

trait WBuildingOps[Self <: WBuilding] extends BuildingOps[Self] with WarpableOps[Self]
trait WBuildingStats extends BuildingStats with WarpableStats {
  override val group = WarpableGroup.Building
}
trait WBuildingCompanion[Self <: WBuilding] extends WBuildingOps[Self] with WBuildingStats
  with BuildingCompanion[Self] with WarpableCompanion[Self]
trait WBuilding extends Building with Warpable {
  type Self <: WBuilding
  type Companion <: WBuildingOps[Self] with WBuildingStats
}