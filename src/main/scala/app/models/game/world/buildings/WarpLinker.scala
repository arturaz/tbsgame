package app.models.game.world.buildings

import app.models.game.Player
import app.models.game.world._

object WarpLinkerStats extends WBuildingStats with WarpableStats
with WarpableCompanion[WarpLinker]
with EmptySpaceWarpableCompanion[WarpLinker] {
  override val maxHp = HP(100)
  override val warpTime = WarpTime(1)
  override val cost = Resources(4)
  override val visibility = RectDistance(5)
  override val warpGiven = visibility
  override val kind = WObjKind.Light

  override def warp(owner: Player, position: Vect2) = WarpLinker(position, owner)
}

case class WarpLinkerOps(self: WarpLinker) extends WarpableOps[WarpLinker] {
  override def setWarpState(newState: WarpTime) = self.copy(warpState = newState)
  override def withNewHp(hp: HP) = self.copy(hp = hp)
}