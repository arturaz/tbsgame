package app.models.game.world.units

import app.models.game.{Population, Player}
import app.models.game.world._

object RayShipStats extends WFighterUnitStats with EmptySpaceWarpableCompanion[RayShip]
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
}

case class RayShipOps(self: RayShip) extends WFighterUnitOps[RayShip] {
  override protected def withAttacksLeft(value: Attacks) =
    self.copy(attacksLeft = value)
  override def withNewHp(hp: HP) = self.copy(hp = hp)
  override def withNewXP(xp: XP) = self.copy(xp = xp)
  override protected def setMoveValues(
    position: Vect2, movementLeft: Movement
  ) = self.copy(position = position, movementLeft = movementLeft)
  override def setWarpState(newState: WarpTime) = self.copy(warpState = newState)
}