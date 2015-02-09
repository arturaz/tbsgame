package app.models.game.world.units

import app.models.game.{Population, Player}
import app.models.game.world._

object FortressStats extends WFighterUnitStats with EmptySpaceWarpableCompanion[Fortress]
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

  override def warp(owner: Player, position: Vect2) = Fortress(position, owner)
}

case class FortressOps(self: Fortress) extends WFighterUnitOps[Fortress] {
  override protected def withNewHp(hp: HP) = self.copy(hp = hp)
  override protected def withAttacksLeft(value: Attacks) = self.copy(attacksLeft = value)
  override protected def withNewXP(value: XP) = self.copy(xp = value)
  override protected def setMoveValues(position: Vect2, movementLeft: Movement) =
    self.copy(position = position, movementLeft = movementLeft)
  override def setWarpState(newState: WarpTime) = self.copy(warpState = newState)
}