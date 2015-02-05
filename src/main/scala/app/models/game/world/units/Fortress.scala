package app.models.game.world.units

import app.models.game.{Population, Player}
import app.models.game.world._

object Fortress extends WFighterUnitCompanion[Fortress]
{
  override val maxHp = HP(185)
  override val attack = Atk(25)
  override val attacks = Attacks(2)
  override val attackRange = RadialDistance.Three
  override val movement = Movement.fromTiles(2)
  override val cost = Resources(10)
  override val warpTime = WarpTime(0)
  override val populationCost = Population(2)
  override val kind = WObjKind.Heavy
  override val visibility = RectDistance(5)

  override protected def setMoveValues(
    position: Vect2, movementLeft: Movement
  )(self: Fortress) = self.copy(position = position, movementLeft = movementLeft)
  override def setWarpState(newState: WarpTime)(self: Fortress) =
    self.copy(warpState = newState)
  override def withNewHp(hp: HP)(self: Fortress) = self.copy(hp = hp)
  override def withNewXP(xp: XP)(self: Fortress) = self.copy(xp = xp)
  override protected def withAttacksLeft(value: Attacks)(self: Fortress) =
    self.copy(attacksLeft = value)
  override def warp(owner: Player, position: Vect2) = Fortress(position, owner)
}

case class Fortress(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=Fortress.maxHp, xp: XP=Fortress.InitialXP,
  attacksLeft: Attacks=Fortress.InitialAttacks,
  movementLeft: Movement=Fortress.movement,
  warpState: WarpTime=Fortress.InitialWarpState
) extends WFighterUnit {
  override type Self = Fortress
  override type Companion = Fortress.type
  override def self = this
  override def companion = Fortress
}
