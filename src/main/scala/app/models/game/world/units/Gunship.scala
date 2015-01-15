package app.models.game.world.units

import app.models.game.{Population, Player}
import app.models.game.world._

object Gunship extends WUnitCompanion[Gunship] with FighterCompanion[Gunship] {
  override val maxHp = HP(90)
  override val attack = Atk(70)
  override val attacks = Attacks(4)
  override val attackRange = RadialDistance.Three
  override val cost = Resources(6)
  override val populationCost = Population(4)
  override val warpTime = WarpTime(0)
  override val kind = WObjKind.Heavy
  override val visibility = RectDistance(3)
  override val movement = Movement.fromTiles(6)

  override protected def withMovedOrAttacked(value: Boolean)(self: Gunship) =
    self.copy(movedOrAttacked = value)
  override def withNewHp(hp: HP)(self: Gunship) = self.copy(hp = hp)
  override def withNewXP(xp: XP)(self: Gunship) = self.copy(xp = xp)
  override protected def withAttacksLeft(value: Attacks)(self: Gunship) =
    self.copy(attacksLeft = value)
  override protected def setMoveValues(
    position: Vect2, movementLeft: Movement
  )(self: Gunship) = self.copy(position = position, movementLeft = movementLeft)
  override def setWarpState(newState: WarpTime)(self: Gunship) =
    self.copy(warpState = newState)
  override def warp(owner: Player, position: Vect2) = Gunship(position, owner)
}

case class Gunship(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=Gunship.maxHp, xp: XP=Gunship.InitialXP,
  attacksLeft: Attacks=Gunship.InitialAttacks,
  movementLeft: Movement=Gunship.movement,
  warpState: WarpTime=Gunship.InitialWarpState,
  movedOrAttacked: Boolean=Gunship.InitialMovedOrAttacked
) extends WUnit with Fighter {
  override type Self = Gunship
  override type Companion = Gunship.type
  override def self = this
  override def companion = Gunship
}