package app.models.world.buildings

import app.models.Player
import app.models.world._

object WarpLinker extends BuildingStats with EmptySpaceWarpableStats[WarpLinker] {
  override val maxHp: Int = 2
  override val size: Vect2 = Vect2(1, 1)
  override val warpTime: Int = 2
  override val cost: Int = 3

  override def warp(owner: Player, position: Vect2) =
    WarpLinker(position, owner)
}

case class WarpLinker(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: Int=WarpLinker.maxHp,
  warpState: Int=WarpLinker.InitialWarpState
) extends PlayerBuilding with Warpable {
  override def stats = WarpLinker

  override protected def advanceWarpState(self: Self, newState: Int) =
    copy(warpState = newState)
  override protected def withNewHp(hp: Int) = copy(hp = hp)
  override protected def self = this
  override type Self = WarpLinker
  override type Stats = WarpLinker.type
}