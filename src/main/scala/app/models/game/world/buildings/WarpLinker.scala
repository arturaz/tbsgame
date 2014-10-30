package app.models.game.world.buildings

import app.models.game.Player
import app.models.game.world._

object WarpLinker extends BuildingCompanion[WarpLinker]
with EmptySpaceWarpableCompanion[WarpLinker] {
  override val maxHp = HP(2)
  override val warpTime = WarpTime(2)
  override val cost = Resources(12)
  override val defense = 2 to 5
  override val visibility = 5
  override val warpGiven = RectDistance(visibility)

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
) extends PlayerBuilding with Warpable {
  type Self = WarpLinker
  def self = this
  override def companion = WarpLinker
  override type Companion = WarpLinker.type
}