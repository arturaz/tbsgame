package app.models.game.world.buildings

import app.models.game.world._

trait BuildingOps[Self] extends OwnedObjOps[Self] {
}

trait BuildingStats extends OwnedObjStats {
  override val visibility = 3
}

trait BuildingCompanion[Self] extends BuildingOps[Self] with BuildingStats

/* Building existing in game world. */
trait Building extends OwnedObj {
  type Companion <: BuildingOps[Self] with BuildingStats
}

trait PlayerBuilding extends Building with PlayerObj
trait HumanBuilding extends Building with HumanObj
trait BotBuilding extends Building with BotObj