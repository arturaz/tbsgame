package app.models.world.units

import app.models.world._

trait WUnitStats extends FactionObjStats with MovableWObjectStats with WarpableStats

/* World unit */
trait WUnit extends FactionObj with MovableWObject with Warpable {
  type Stats <: WUnitStats
}