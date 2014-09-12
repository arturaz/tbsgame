package app.models.world.buildings

import app.models.world.{FactionObjStats, FactionObj, WObject}

trait BuildingStats extends FactionObjStats {
  override val visibility = 3
  override val defense = emptyRange
}

/* Building existing in game world. */
trait Building extends WObject with FactionObj