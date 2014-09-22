package app.models.world.buildings

import app.models.Player
import app.models.world._

object LaserTower extends BuildingStats with FighterStats
with EmptySpaceWarpableStats[LaserTower] {
  override def warp(owner: Player, position: Vect2) =
    LaserTower(position, owner)

  override val maxHp: Int = 3
  override val size: Vect2 = Vect2.one
  override val warpTime: Int = 1
  override val cost: Int = 10
  override val attack: Range = 2 to 14
  override val attackRange: TileDistance = TileDistance(5)
  override val moveAttackActionsNeeded: Int = 2
}

case class LaserTower(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: Int=LaserTower.maxHp,
  warpState: Int=LaserTower.InitialWarpState, hasAttacked: Boolean=false,
  movedOrAttacked: Boolean=LaserTower.InitialMovedOrAttacked
) extends PlayerBuilding with Warpable with Fighter {
  override protected def advanceWarpState(self: Self, newState: Int) =
    self.copy(warpState=newState)
  override protected def withNewHp(hp: Int) = copy(hp = hp)
  override def stats = LaserTower
  override protected def self = this
  override type Self = LaserTower
  override type Stats = LaserTower.type
  override protected def attacked(value: Boolean)(self: Self) =
    self.copy(hasAttacked=value)
  override protected def withMovedOrAttacked(value: Boolean)(self: Self) =
    self.copy(movedOrAttacked = value)
}