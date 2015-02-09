package app.models.game.world.units

import app.models.game.world._

trait WUnitStats extends OwnedObjStats with MovableStats with WarpableStats {
  override val group = WarpableGroup.Unit
}

trait WFighterUnitStats extends WUnitStats with FighterStats

trait WUnitImpl extends MovableImpl with WarpableImpl {
  type Stats <: WUnitStats
}

trait WUnitOps[Self <: WUnit]
extends OwnedObjOps[Self] with WarpableOps[Self] with MovableOps[Self]

trait WFighterUnitOps[Self <: WUnit with Fighter]
extends WUnitOps[Self] with FighterOps[Self]