package app.models.game.world.units

import app.models.game.{Population, Player}
import app.models.game.world._

object RocketFrigateStats extends WFighterUnitStats
with EmptySpaceWarpableCompanion[RocketFrigate]
{
  override val maxHp = HP(30)
  override val attack = Atk(210)
  override val attacks = Attacks(1)
  override val attackRange = RadialDistance.Eight
  override val movement = Movement.fromTiles(8)
  override val visibility = RectDistance(2)
  override val cost = Resources(8)
  override val populationCost = Population(3)
  override val warpTime = WarpTime(0)
  override val kind = WObjKind.Light

  override def warp(owner: Player, position: Vect2) = RocketFrigate(position, owner)
}

case class RocketFrigateOps(self: RocketFrigate) extends WFighterUnitOps[RocketFrigate] {
  override protected def setMoveValues(
    position: Vect2, movementLeft: Movement
  ) = self.copy(position = position, movementLeft = movementLeft)
  override def setWarpState(newState: WarpTime) =
    self.copy(warpState = newState)
  override protected def withAttacksLeft(value: Attacks) =
    self.copy(attacksLeft = value)
  override def withNewHp(hp: HP) = self.copy(hp = hp)
  override def withNewXP(xp: XP) = self.copy(xp = xp)
}