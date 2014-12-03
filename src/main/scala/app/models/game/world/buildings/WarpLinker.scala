package app.models.game.world.buildings

import app.models.game.Player
import app.models.game.world._

object WarpLinker extends WBuildingCompanion[WarpLinker]
with EmptySpaceWarpableCompanion[WarpLinker] {
  override val maxHp = HP(280)
  override val warpTime = WarpTime(1)
  override val cost = Resources(8)
  override val defense = 0 to 0
  override val visibility = RectDistance(5)
  override val warpGiven = visibility
  override val kind = WObjKind.Light

  override def warp(owner: Player, position: Vect2) =
    WarpLinker(position, owner)
  override def setWarpState(newState: WarpTime)(self: WarpLinker) =
    self.copy(warpState = newState)
  override def withNewHp(hp: HP)(self: WarpLinker) = self.copy(hp = hp)
}

case class WarpLinker(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=WarpLinker.maxHp,
  warpState: WarpTime=WarpLinker.InitialWarpState
) extends PlayerBuilding with WBuilding {
  type Self = WarpLinker
  def self = this
  override def companion = WarpLinker
  override type Companion = WarpLinker.type
}