package app.models.game.world.buildings

import app.models.game.Player
import app.models.game.world._

trait WarpLinkerStatsImpl extends EmptySpaceWarpableCompanion[WarpLinker] {
_: WarpLinkerStats.type =>
  override val maxHp = HP(100)
  override val warpTime = WarpTime(1)
  override val cost = Resources(4)
  override val visibility = RectDistance(2)
  override val warpGiven = visibility
  override val kind = WObjKind.Light
  override val needsWarpZoneToWarp = false

  override def warp(owner: Player, position: Vect2) = WarpLinker(position, owner)
}

case class WarpLinkerOps(self: WarpLinker) extends WarpableOps[WarpLinker] {
  override def setWarpState(newState: WarpTime) = self.copy(warpState = newState)
  override def withNewHp(hp: HP) = self.copy(hp = hp)
}