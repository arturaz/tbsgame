package app.models.game.world.units

import app.models.game.Player
import app.models.game.world._

// slow, armor, -range, +dmg
object Fortress extends WUnitCompanion[Fortress] with FighterCompanion[Fortress] {
  override val attackRange = TileDistance(5)
  override val attacks = Attacks(3)
  override val attack = 6 to 10
  override val movement = TileDistance(1)
  override val cost = Resources(35)
  override val warpTime = WarpTime(2)
  override val kind = WObjKind.Heavy
  override val visibility = 5
  override val maxHp = HP(30)
  override val defense = 6 to 10

  override protected def setMoveValues(
    position: Vect2, movementLeft: TileDistance
  )(self: Fortress) = self.copy(position = position, movementLeft = movementLeft)
  override def setWarpState(newState: WarpTime)(self: Fortress) =
    self.copy(warpState = newState)
  override protected def withMovedOrAttacked(value: Boolean)(self: Fortress) =
    self.copy(movedOrAttacked = value)
  override def withNewHp(hp: HP)(self: Fortress) = self.copy(hp = hp)
  override protected def withAttacksLeft(value: Attacks)(self: Fortress) =
    self.copy(attacksLeft = value)
  override def warp(owner: Player, position: Vect2) = Fortress(position, owner)
}

case class Fortress(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=Fortress.maxHp,
  attacksLeft: Attacks=Fortress.InitialAttacks,
  movementLeft: TileDistance=Fortress.movement,
  warpState: WarpTime=Fortress.InitialWarpState,
  movedOrAttacked: Boolean=Fortress.InitialMovedOrAttacked
) extends WUnit with Fighter {
  override type Self = Fortress
  override type Companion = Fortress.type
  override def self = this
  override def companion = Fortress
}
