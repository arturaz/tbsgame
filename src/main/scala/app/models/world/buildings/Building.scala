package app.models.world.buildings

import app.models.world._

trait BuildingOps[Self] extends SizedWObjectOps with OwnedObjOps[Self] {
_: SizedWObjectStats =>
}

trait BuildingStats extends SizedWObjectStats with OwnedObjStats {
  override val visibility = 3
}

trait BuildingCompanion[Self] extends BuildingOps[Self] with BuildingStats

/* Building existing in game world. */
trait Building extends SizedWObject with OwnedObj {
  type Companion <: BuildingOps[Self] with BuildingStats
}

trait PlayerBuilding extends Building with PlayerObj
trait HumanBuilding extends Building with HumanObj
trait BotBuilding extends Building with BotObj