package app.models.game.world.buildings

import app.models.game.Population
import app.models.game.world._

trait BuildingStatsImpl { _: BuildingStats =>
  override val visibility = RectDistance(3)
}

/* Warpable building */
trait WBuildingStatsImpl { _: WBuildingStats =>
  override val group = WarpableGroup.Building
  override val populationCost = Population(0)
}

/* Building existing in game world. */
trait BuildingImpl extends OwnedObjImpl with MobilityStatic {
  type Stats <: BuildingStats
}