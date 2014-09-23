package app.models.world.units

import app.models.Player
import app.models.world._

object Wasp extends WUnitCompanion[Wasp] with FighterCompanion[Wasp] {
  override val attack: Range = 1 to 6
  override val defense: Range = 2 to 7
  override val attackRange = TileDistance(3)
  override val movement = TileDistance(4)
  override val visibility: Int = 4
  override val maxHp: Int = 1
  override val warpTime: Int = 0
  override val cost: Int = 0

  override def warp(owner: Player, position: Vect2) = Wasp(position, owner)
  override def setWarpState(newState: Int)(self: Wasp) =
    self.copy(warpState = newState)
  override def setMoveValues(position: Vect2, movementLeft: TileDistance)(self: Wasp) =
    self.copy(position = position, movementLeft = movementLeft)
  override def withMovedOrAttacked(value: Boolean)(self: Wasp) =
    self.copy(movedOrAttacked = value)
  override def withNewHp(hp: Int)(self: Wasp) =
    self.copy(hp = hp)
  override def attacked(value: Boolean)(self: Wasp) = self.copy(hasAttacked = value)
}

case class Wasp(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: Int=Wasp.maxHp, hasAttacked: Boolean=false,
  movementLeft: TileDistance=Wasp.movement, warpState: Int=Wasp.InitialWarpState,
  movedOrAttacked: Boolean=Corvette.InitialMovedOrAttacked
) extends WUnit with Fighter {
  type Self = Wasp
  type Companion = Wasp.type
  override protected def self = this
  override def companion = Wasp
}
