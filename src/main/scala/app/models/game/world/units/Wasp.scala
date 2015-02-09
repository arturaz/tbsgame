package app.models.game.world.units

import app.models.game.{Population, Player}
import app.models.game.world._

object WaspStats extends WFighterUnitStats with EmptySpaceWarpableCompanion[Wasp] {
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
}

case class WaspOps(self: Wasp) extends WFighterUnitOps[Wasp] {
  override def setWarpState(newState: WarpTime) =
    self.copy(warpState = newState)
  override def setMoveValues(position: Vect2, movementLeft: Movement) =
    self.copy(position = position, movementLeft = movementLeft)
  override def withNewHp(hp: HP) = self.copy(hp = hp)
  override def withNewXP(xp: XP) = self.copy(xp = xp)
  override def withAttacksLeft(value: Attacks) = self.copy(attacksLeft = value)
}
