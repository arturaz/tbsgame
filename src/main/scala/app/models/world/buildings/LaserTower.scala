package app.models.world.buildings

import app.models.Player
import app.models.world._

object LaserTower extends BuildingCompanion[LaserTower]
with ReactiveFighterCompanion[LaserTower]
with EmptySpaceWarpableCompanion[LaserTower] {
  override val maxHp: Int = 3
  override val size: Vect2 = Vect2.one
  override val warpTime: Int = 1
  override val cost: Int = 10
  override val attack: Range = 2 to 14
  override val attackRange: TileDistance = TileDistance(5)

  override def warp(owner: Player, position: Vect2) =
    LaserTower(position, owner)
  override def setWarpState(newState: Int)(self: LaserTower) =
    self.copy(warpState=newState)
  override def attacked(value: Boolean)(self: LaserTower) =
    self.copy(hasAttacked=value)
  override def withMovedOrAttacked(value: Boolean)(self: LaserTower) =
    self.copy(movedOrAttacked = value)
  override def withNewHp(hp: Int)(self: LaserTower) = self.copy(hp = hp)
}

case class LaserTower(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: Int=LaserTower.maxHp,
  warpState: Int=LaserTower.InitialWarpState, hasAttacked: Boolean=false,
  movedOrAttacked: Boolean=LaserTower.InitialMovedOrAttacked
) extends PlayerBuilding with Warpable with ReactiveFighter {
  override def companion = LaserTower
  override protected def self = this
  override type Self = LaserTower
  override type Companion = LaserTower.type
}