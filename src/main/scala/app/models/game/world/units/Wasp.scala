package app.models.game.world.units

import app.models.game.Player
import app.models.game.world._

object Wasp extends WUnitCompanion[Wasp] with FighterCompanion[Wasp] {
  override val maxHp = HP(130)
  override val attack = Atk(30)
  override val attacks = Attacks(2)
  override val attackRange = TileDistance(3)
  override val movement = TileDistance(3)
  override val visibility = RectDistance(4)
  override val warpTime = WarpTime(0)
  override val cost = Resources(10)
  override val kind = WObjKind.Medium

  override def warp(owner: Player, position: Vect2) = Wasp(position, owner)
  override def setWarpState(newState: WarpTime)(self: Wasp) =
    self.copy(warpState = newState)
  override def setMoveValues(position: Vect2, movementLeft: TileDistance)(self: Wasp) =
    self.copy(position = position, movementLeft = movementLeft)
  override def withMovedOrAttacked(value: Boolean)(self: Wasp) =
    self.copy(movedOrAttacked = value)
  override def withNewHp(hp: HP)(self: Wasp) =
    self.copy(hp = hp)
  override def withAttacksLeft(value: Attacks)(self: Wasp) = self.copy(attacksLeft = value)
}

case class Wasp(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=Wasp.maxHp,
  attacksLeft: Attacks=Wasp.InitialAttacks,
  movementLeft: TileDistance=Wasp.movement, warpState: WarpTime=Wasp.InitialWarpState,
  movedOrAttacked: Boolean=Wasp.InitialMovedOrAttacked
) extends WUnit with Fighter {
  type Self = Wasp
  type Companion = Wasp.type
  def self = this
  override def companion = Wasp
}
