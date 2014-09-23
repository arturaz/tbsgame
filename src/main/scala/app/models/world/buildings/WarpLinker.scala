package app.models.world.buildings

import app.models.Player
import app.models.world._

object WarpLinker extends BuildingCompanion[WarpLinker]
with EmptySpaceWarpableCompanion[WarpLinker] {
  override val maxHp: Int = 2
  override val size: Vect2 = Vect2(1, 1)
  override val warpTime: Int = 2
  override val cost: Int = 3

  override def warp(owner: Player, position: Vect2) =
    WarpLinker(position, owner)
  override def setWarpState(newState: Int)(self: WarpLinker) =
    self.copy(warpState = newState)
  override def withNewHp(hp: Int)(self: WarpLinker) = self.copy(hp = hp)
}

case class WarpLinker(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: Int=WarpLinker.maxHp,
  warpState: Int=WarpLinker.InitialWarpState
) extends PlayerBuilding with Warpable {
  override def companion = WarpLinker
  override protected def self = this
  override type Self = WarpLinker
  override type Companion = WarpLinker.type
}