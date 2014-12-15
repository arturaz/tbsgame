package app.models.game.world.buildings

import app.models.game.events.{OwnerChangeEvt, Evented}
import app.models.game.{Team, Actions, Owner}
import app.models.game.world._

object VPTower extends BuildingCompanion[VPTower] with GivingActionsCompanion[VPTower]
with RespawnsOnDestructionCompanion[VPTower, VPTower#OwnerType] with SizedWObjectCompanion
{
  override val maxHp = HP(450)
  override val visibility = RectDistance(5)
  override val warpGiven = visibility
  override val actionsGiven = Actions(1)
  override val kind = WObjKind.Medium
  override val size = Vect2(2, 2)

  override def withNewHp(hp: HP)(self: VPTower) = self.copy(hp = hp)
  override val hpAfterRespawn = maxHp
  override def withNewOwner(owner: Team)(self: VPTower) = self.copy(owner = owner)

}

case class VPTower(
  position: Vect2, owner: Team,
  id: WObject.Id=WObject.newId, hp: HP=VPTower.maxHp
) extends TeamBuilding with GivingActions with RespawnsOnDestruction with SizedWObject {
  override type OwnerType = Team
  override type Self = VPTower
  override type Companion = VPTower.type
  override def self = this
  override def companion = VPTower
  override def ownerAfterRespawn(attacker: Owner) = attacker.team
}
