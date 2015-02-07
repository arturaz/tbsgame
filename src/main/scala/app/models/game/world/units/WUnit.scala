package app.models.game.world.units

import app.models.game.world._

trait WUnitOps[Self <: WUnit] extends OwnedObjOps[Self] with MovableWObjectOps[Self]
with WarpableOps[Self] with EmptySpaceWarpableOps[Self]
{ _: WUnitStats => }

trait WUnitStats extends OwnedObjStats with MovableStats
with WarpableStats {
  override val group = WarpableGroup.Unit
}

trait WUnitCompanion[Self <: WUnit] extends WUnitOps[Self] with WUnitStats
with EmptySpaceWarpableCompanion[Self]

/* World unit */
trait WUnit extends PlayerObj with Movable with Warpable { thiz =>
  type Self >: thiz.type <: WUnit
  type Companion <: WUnitOps[Self] with WUnitStats
}

trait WFighterUnitOps[Self <: WFighterUnit] extends WUnitOps[Self] with FighterOps[Self]
{ _: WUnitStats => }

trait WFighterUnitStats extends WUnitStats with FighterStats

trait WFighterUnitCompanion[Self <: WFighterUnit] extends WUnitCompanion[Self]
with WFighterUnitOps[Self]
with WFighterUnitStats

trait WFighterUnit extends WUnit with Fighter { thiz =>
  type Self >: thiz.type <: WFighterUnit
  type Companion <: WFighterUnitOps[Self] with WFighterUnitStats
}