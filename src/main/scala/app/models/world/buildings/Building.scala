package app.models.world.buildings

import app.models.world._

trait BuildingStats extends SizedWObjectStats with OwnedObjStats {
  override val visibility = 3
  override val defense = emptyRange
}

/* Building existing in game world. */
trait Building extends SizedWObject with OwnedObj {
  type Stats <: BuildingStats
}

trait PlayerBuilding extends Building with PlayerObj