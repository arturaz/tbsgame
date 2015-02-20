package app.models.game.world.units

import app.models.game.{Population, Player}
import app.models.game.world._

object GunshipStats extends WFighterUnitStats with EmptySpaceWarpableCompanion[Gunship]
{
  override val maxHp = HP(110)
  override val attack = Atk(75)
  override val attacks = Attacks(3)
  override val attackRange = RadialDistance.Three
  override val cost = Resources(9)
  override val populationCost = Population(4)
  override val kind = WObjKind.Heavy
  override val visibility = RectDistance(3)
  override val movement = Movement.fromTiles(8)

  override def warp(owner: Player, position: Vect2) = Gunship(position, owner)
}

case class GunshipOps(self: Gunship) extends WFighterUnitOps[Gunship] {
  override protected def withNewHp(hp: HP) = self.copy(hp = hp)
  override protected def withNewXP(xp: XP) = self.copy(xp = xp)
  override protected def withAttacksLeft(value: Attacks) = self.copy(attacksLeft = value)
  override protected def setMoveValues(
    position: Vect2, movementLeft: Movement
  ) = self.copy(position = position, movementLeft = movementLeft)
  override def setWarpState(newState: WarpTime) = self.copy(warpState = newState)
}