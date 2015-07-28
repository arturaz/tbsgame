package app.models.game.world

import app.models.game.world.buildings.VPTowerOps

import scala.language.implicitConversions

trait GivingVictoryPointsStatsImpl { _: GivingVictoryPointsStats =>
  /* Victory points given each turn */
  val vpsGiven: VPS
}

trait GivingVictoryPointsImpl extends OwnedObjImpl {
  type Stats <: GivingVictoryPointsStats
}

trait GivingVictoryPointsOps[Self <: GivingVictoryPoints] extends OwnedObjOps[Self] {
  def giveVP(world: World) = world.addVps(self.owner, self.stats.vpsGiven)

  final def givingVPsTeamTurnStarted(world: World) = giveVP(world)
}

trait ToGivingVictoryPointsOps {
  implicit def toGivingVictoryPointsOps[A <: GivingVictoryPoints](a: A)
  : GivingVictoryPointsOps[A] = (a match {
    case o: VPTower => VPTowerOps(o)
  }).asInstanceOf[GivingVictoryPointsOps[A]]
}