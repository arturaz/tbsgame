package app.models.game.world.units

import app.models.game.Player
import app.models.game.world._

object Gunship extends WUnitCompanion[Gunship] with FighterCompanion[Gunship] {
  override val attackRange = TileDistance(3)
  override val attacks = Attacks(4)
  override val attack = 4 to 8
  override val cost = Resources(20)
  override val warpTime = WarpTime(0)
  override val defense = 1 to 2
  override val kind = WObjKind.Heavy
  override val visibility = 3
  override val maxHp = HP(8)
  override val movement = TileDistance(4)

  override protected def withMovedOrAttacked(value: Boolean)(self: Gunship) =
    self.copy(movedOrAttacked = value)
  override def withNewHp(hp: HP)(self: Gunship) = self.copy(hp = hp)
  override protected def withAttacksLeft(value: Attacks)(self: Gunship) =
    self.copy(attacksLeft = value)
  override protected def setMoveValues(
    position: Vect2, movementLeft: TileDistance
  )(self: Gunship) = self.copy(position = position, movementLeft = movementLeft)
  override def setWarpState(newState: WarpTime)(self: Gunship) =
    self.copy(warpState = newState)
  override def warp(owner: Player, position: Vect2) = Gunship(position, owner)
}

case class Gunship(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=Gunship.maxHp,
  attacksLeft: Attacks=Gunship.InitialAttacks,
  movementLeft: TileDistance=Gunship.movement,
  warpState: WarpTime=Gunship.InitialWarpState,
  movedOrAttacked: Boolean=Gunship.InitialMovedOrAttacked
) extends WUnit with Fighter {
  override type Self = Gunship
  override type Companion = Gunship.type
  override def self = this
  override def companion = Gunship
}