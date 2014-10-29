package app.models.game.world.units

import app.models.game.world._

trait WUnitOps[Self <: WUnit] extends OwnedObjOps[Self] with MovableWObjectOps[Self]
with WarpableOps[Self] with EmptySpaceWarpableOps[Self]
{ _: WUnitStats => }

trait WUnitStats extends OwnedObjStats with MovableWObjectStats
with WarpableStats

trait WUnitCompanion[Self <: WUnit] extends WUnitOps[Self] with WUnitStats
with EmptySpaceWarpableCompanion[Self]

/* World unit */
trait WUnit extends PlayerObj with MovableWObject with Warpable {
  type Self <: WUnit
  type Companion <: WUnitOps[Self] with WUnitStats
}