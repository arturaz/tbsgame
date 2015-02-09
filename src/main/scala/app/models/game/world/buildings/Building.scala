package app.models.game.world.buildings

import app.models.game.Population
import app.models.game.world._

trait BuildingStats extends OwnedObjStats {
  override val visibility = RectDistance(3)
}

/* Warpable building */
trait WBuildingStats extends BuildingStats with WarpableStats {
  override val group = WarpableGroup.Building
  override val populationCost = Population(0)
}

/* Building existing in game world. */
trait BuildingImpl extends OwnedObjImpl with Mobility[Mobility.Static.type] {
  type Stats <: BuildingStats
}