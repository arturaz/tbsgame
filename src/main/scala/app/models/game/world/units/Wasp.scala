package app.models.game.world.units

import app.models.game.{Population, Player}
import app.models.game.world._

object Wasp extends WUnitCompanion[Wasp] with FighterCompanion[Wasp] {
  override val maxHp = HP(80)
  override val attack = Atk(56)
  override val attacks = Attacks(2)
  override val attackRange = RadialDistance.Three
  override val movement = Movement.fromTiles(3)
  override val visibility = RectDistance(4)
  override val warpTime = WarpTime(0)
  override val cost = Resources(10)
  override val populationCost = Population(2)
  override val kind = WObjKind.Medium

  override def warp(owner: Player, position: Vect2) = Wasp(position, owner)
  override def setWarpState(newState: WarpTime)(self: Wasp) =
    self.copy(warpState = newState)
  override def setMoveValues(position: Vect2, movementLeft: Movement)(self: Wasp) =
    self.copy(position = position, movementLeft = movementLeft)
  override def withNewHp(hp: HP)(self: Wasp) = self.copy(hp = hp)
  override def withNewXP(xp: XP)(self: Wasp) = self.copy(xp = xp)
  override def withAttacksLeft(value: Attacks)(self: Wasp) = self.copy(attacksLeft = value)
}

case class Wasp(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=Wasp.maxHp, xp: XP=Wasp.InitialXP,
  attacksLeft: Attacks=Wasp.InitialAttacks,
  movementLeft: Movement=Wasp.movement, warpState: WarpTime=Wasp.InitialWarpState
) extends WUnit with Fighter {
  type Self = Wasp
  type Companion = Wasp.type
  def self = this
  override def companion = Wasp
}
