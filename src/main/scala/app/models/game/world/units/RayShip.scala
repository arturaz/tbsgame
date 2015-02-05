package app.models.game.world.units

import app.models.game.{Population, Player}
import app.models.game.world._

/**
 * Created by arturas on 2014-11-04.
 */
object RayShip extends WFighterUnitCompanion[RayShip]
{
  override val maxHp = HP(80)
  override val attack = Atk(65)
  override val attacks = Attacks(1)
  override val attackRange = RadialDistance.Seven
  override val movement = Movement.fromTiles(2)
  override val visibility = RectDistance(6)
  override val cost = Resources(10)
  override val populationCost = Population(2)
  override val warpTime = WarpTime(0)
  override val kind = WObjKind.Light

  override def warp(owner: Player, position: Vect2) = RayShip(position, owner)
  override protected def withAttacksLeft(value: Attacks)(self: RayShip) =
    self.copy(attacksLeft = value)
  override def withNewHp(hp: HP)(self: RayShip) = self.copy(hp = hp)
  override def withNewXP(xp: XP)(self: RayShip) = self.copy(xp = xp)
  override def setWarpState(newState: WarpTime)(self: RayShip) =
    self.copy(warpState = newState)
  override protected def setMoveValues(
    position: Vect2, movementLeft: Movement
  )(self: RayShip) = self.copy(position = position, movementLeft = movementLeft)
}

case class RayShip(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=RayShip.maxHp, xp: XP=RayShip.InitialXP,
  attacksLeft: Attacks=RayShip.InitialAttacks,
  movementLeft: Movement=RayShip.movement,
  warpState: WarpTime=RayShip.InitialWarpState
) extends WFighterUnit {
  type Self = RayShip
  type Companion = RayShip.type
  def self = this
  def companion = RayShip
}