package app.models.game.world.units

import app.models.game.world._

trait WUnitStatsImpl { _: WUnitStats =>
  override val group = WarpableGroup.Unit
  override val warpTime = WarpTime(1)
}

trait WUnitImpl extends MovableImpl with WarpableImpl {
  type Stats <: WUnitStats
}

trait WUnitOps[Self <: WUnit]
extends OwnedObjOps[Self] with WarpableOps[Self] with MovableOps[Self]

trait WFighterUnitOps[Self <: WUnit with Fighter]
extends WUnitOps[Self] with FighterOps[Self]