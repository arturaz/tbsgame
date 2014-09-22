package app.models.world.units

import app.models.Player
import app.models.world._

object Wasp extends WUnitStats[Wasp] with FighterStats {
  override val attack: Range = 1 to 6
  override val defense: Range = 2 to 7
  override val attackRange = TileDistance(3)
  override val movement = TileDistance(4)
  override val visibility: Int = 4
  override val maxHp: Int = 1
  override val warpTime: Int = 0
  override val cost: Int = 0

  override def warp(owner: Player, position: Vect2) = Wasp(position, owner)
}

case class Wasp(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: Int=Wasp.maxHp, hasAttacked: Boolean=false,
  movementLeft: TileDistance=Wasp.movement, warpState: Int=Wasp.InitialWarpState,
  movedOrAttacked: Boolean=Corvette.InitialMovedOrAttacked
) extends WUnit with Fighter {
  type Self = Wasp
  type Stats = Wasp.type
  override protected def self = this

  override def stats = Wasp

  override protected def setMoveValues(
    target: Vect2, movementLeft: TileDistance
  )(self: Self) =
    self.copy(position = target, movementLeft = movementLeft)
  override protected def advanceWarpState(self: Self, newState: Int) =
    self.copy(warpState = newState)
  override protected def attacked(value: Boolean)(self: Self) =
    self.copy(hasAttacked = value)
  override protected def withNewHp(hp: Int) = copy(hp = hp)
  override protected def withMovedOrAttacked(value: Boolean)(self: Self) =
    self.copy(movedOrAttacked = value)
}
