package app.models.game.world.buildings

import app.models.game.{Actions, Player}
import app.models.game.world._

object LaserTower extends BuildingCompanion[LaserTower]
with EmptySpaceWarpableCompanion[LaserTower]
with ReactiveFighterCompanion[LaserTower] {
  override val maxHp = HP(3)
  override val warpTime = WarpTime(1)
  override val cost = Resources(10)
  override val attack = 2 to 14
  override val attackRange = TileDistance(5)
  override val defense = 3 to 8
  override val moveAttackActionsNeeded = Actions(2)

  override def warp(owner: Player, position: Vect2) =
    LaserTower(position, owner)
  override def setWarpState(newState: WarpTime)(self: LaserTower) =
    self.copy(warpState=newState)
  override def attacked(value: Boolean)(self: LaserTower) =
    self.copy(hasAttacked=value)
  override def withMovedOrAttacked(value: Boolean)(self: LaserTower) =
    self.copy(movedOrAttacked = value)
  override def withNewHp(hp: HP)(self: LaserTower) = self.copy(hp = hp)

}

case class LaserTower(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=LaserTower.maxHp,
  warpState: WarpTime=LaserTower.InitialWarpState, hasAttacked: Boolean=false,
  movedOrAttacked: Boolean=LaserTower.InitialMovedOrAttacked
) extends PlayerBuilding with Warpable with ReactiveFighter {
  type Self = LaserTower
  def self = this
  override def companion = LaserTower
  override type Companion = LaserTower.type
}