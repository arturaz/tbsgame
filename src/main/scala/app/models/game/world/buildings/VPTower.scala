package app.models.game.world.buildings

import app.models.game.world._
import app.models.game.{Actions, Owner, Team}

trait VPTowerStatsImpl { _: VPTowerStats.type =>
  override val maxHp = HP(450)
  override val visibility = RectDistance(3)
  override val warpGiven = RectDistance(0)
  override val actionsGiven = Actions(2)
  override val size = Vect2(2, 2)
  override val vpsGiven = VPS(0)

  override val hpAfterRespawn = maxHp
}

case class VPTowerOps(self: VPTower)
extends RespawnsOnDestructionOps[VPTower]
with GivingVictoryPointsOps[VPTower]
{
  override protected def withNewHp(hp: HP) = self.copy(hp = hp)
  override protected def withNewOwner(owner: Team) = self.copy(owner = owner)
}

trait VPTowerImpl {
_: Building with GivingActions
with RespawnsOnDestruction with SizedWObject with GivingVictoryPoints =>
  type OwnerType = Team
  override def ownerAfterRespawn(attacker: Owner) = attacker.team
}
