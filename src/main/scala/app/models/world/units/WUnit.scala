package app.models.world.units

import app.models.world._

trait WUnitStats[W <: Warpable] extends OwnedObjStats with MovableWObjectStats
with EmptySpaceWarpableStats[W]

/* World unit */
trait WUnit extends PlayerObj with MovableWObject with Warpable {
  type Self <: WUnit
  type Stats <: WUnitStats[Self]
}