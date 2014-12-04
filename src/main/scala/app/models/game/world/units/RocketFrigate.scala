package app.models.game.world.units

import app.models.game.Player
import app.models.game.world._

object RocketFrigate extends WUnitCompanion[RocketFrigate]
with FighterCompanion[RocketFrigate] {
  override val maxHp = HP(115)
  override val attack = Atk(95)
  override val attacks = Attacks(1)
  override val attackRange = TileDistance(8)
  override val movement = TileDistance(6)
  override val visibility = RectDistance(2)
  override val cost = Resources(6)
  override val warpTime = WarpTime(0)
  override val kind = WObjKind.Light

  override protected def setMoveValues(
    position: Vect2, movementLeft: TileDistance
  )(self: RocketFrigate) = self.copy(position = position, movementLeft = movementLeft)
  override def setWarpState(newState: WarpTime)(self: RocketFrigate) =
    self.copy(warpState = newState)
  override protected def withMovedOrAttacked(value: Boolean)(self: RocketFrigate) =
    self.copy(movedOrAttacked = value)
  override protected def withAttacksLeft(value: Attacks)(self: RocketFrigate) =
    self.copy(attacksLeft = value)
  override def withNewHp(hp: HP)(self: RocketFrigate) = self.copy(hp = hp)
  override def warp(owner: Player, position: Vect2) = RocketFrigate(position, owner)
}

case class RocketFrigate(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=RocketFrigate.maxHp,
  attacksLeft: Attacks=RocketFrigate.InitialAttacks,
  movementLeft: TileDistance=RocketFrigate.movement,
  warpState: WarpTime=RocketFrigate.InitialWarpState,
  movedOrAttacked: Boolean=RocketFrigate.InitialMovedOrAttacked
) extends WUnit with Fighter {
  override type Self = RocketFrigate
  override type Companion = RocketFrigate.type
  override def self = this
  override def companion = RocketFrigate
}
