package app.models.game.world.buildings

import app.models.game.world._
import app.models.game.{Player, Actions, Owner, Team}

trait ActionTowerStatsImpl extends EmptySpaceWarpableCompanion[ActionTower] {
_: ActionTowerStats.type =>
  override val maxHp = HP(150)
  override val visibility = RectDistance(5)
  override val warpGiven = RectDistance(0)
  override val cost = Resources(15)
  override val warpTime = WarpTime(2)
  override val actionsGiven = Actions(3)
  override val kind = WObjKind.Medium

  override def warp(owner: Player, position: Vect2) = ActionTower(position, owner)
}

case class ActionTowerOps(self: ActionTower) extends WarpableOps[ActionTower] {
  override protected def withNewHp(hp: HP) = self.copy(hp = hp)
  override def setWarpState(newState: WarpTime) = self.copy(warpState = newState)
}

trait ActionTowerImpl { _: ActionTower => }
