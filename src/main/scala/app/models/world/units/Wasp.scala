package app.models.world.units

import app.models.Owner
import app.models.world._

object Wasp extends WUnitStats with FighterStats {
  override val attack: Range = 1 to 6
  override val defense: Range = 2 to 7
  override val attackRange = TileDistance(3)
  override val movement = TileDistance(4)
  override val visibility: Int = 4
  override val maxHp: Int = 1
  override val warpTime: Int = 0
  override val cost: Int = 0
}

case class WaspData(id: WObject.Id, position: Vect2)

case class Wasp(
  position: Vect2, owner: Owner,
  id: WObject.Id=WObject.newId, hp: Int=Wasp.maxHp, hasAttacked: Boolean=false,
  movementLeft: TileDistance=Wasp.movement, warpState: Int=Wasp.InitialWarpState
) extends WUnit with Fighter {
  type Self = Wasp
  type Stats = Wasp.type
  override protected def self = this

  override def stats = Wasp

  override protected def setMoveValues(
    self: Self, target: Vect2, movementLeft: TileDistance
  ) = self.copy(position = target, movementLeft = movementLeft)
  override protected def advanceWarpState(self: Self, newState: Int) =
    self.copy(warpState = newState)
  override protected def attacked(self: Self, value: Boolean) =
    self.copy(hasAttacked = value)
  override protected def withNewHp(hp: Int) = copy(hp = hp)
}
