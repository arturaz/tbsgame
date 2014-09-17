package app.models.world.buildings

import app.models.Owner
import app.models.world.{Vect2, WObject, Warpable, WarpableStats}

object WarpLinker extends BuildingStats with WarpableStats {
  override val maxHp: Int = 2
  override val size: Vect2 = Vect2(1, 1)
  override val warpTime: Int = 2
  override val cost: Int = 3
}

case class WarpLinker(
  id: WObject.Id, position: Vect2, owner: Owner,
  hp: Int=WarpLinker.maxHp, warpState: Int=WarpLinker.InitialWarpState
) extends Building with Warpable {
  override def stats = WarpLinker

  override protected def advanceWarpState(self: Self, newState: Int) =
    copy(warpState = newState)
  override protected def withNewHp(hp: Int) = copy(hp = hp)
  override protected def self = this
  override type Self = WarpLinker
  override type Stats = WarpLinker.type
}