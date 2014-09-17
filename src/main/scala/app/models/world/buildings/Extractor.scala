package app.models.world.buildings

import app.models.Owner
import app.models.world.{WObject, Warpable, WarpableStats, Vect2}
import app.models.world.props.Asteroid

object Extractor extends BuildingStats with WarpableStats {
  override val maxHp = 1
  override val size: Vect2 = Vect2(1, 1)
  override val warpTime: Int = 1
  override val cost: Int = 4
}

case class Extractor(
  id: WObject.Id, position: Vect2, asteroid: Asteroid, owner: Owner,
  hp: Int=Extractor.maxHp, warpState: Int=Extractor.InitialWarpState
) extends Building with Warpable {
  override def stats = Extractor

  override protected def advanceWarpState(self: Self, newState: Int) =
    copy(warpState = newState)
  override protected def self = this
  override type Self = Extractor
  override type Stats = Extractor.type
  override protected def withNewHp(hp: Int) = copy(hp = hp)
}
