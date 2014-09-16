package app.models.world.buildings

import app.models.world.{SizedWObjectStats, FactionObj, FactionObjStats, SizedWObject}

trait BuildingStats extends SizedWObjectStats with FactionObjStats {
  override val visibility = 3
  override val defense = emptyRange
}

/* Building existing in game world. */
trait Building extends SizedWObject with FactionObj {
  type Stats <: BuildingStats
}