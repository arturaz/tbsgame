package app.models.game.world.units

import app.models.game.{Population, Player}
import app.models.game.world._

object ScoutStats extends WUnitStats with EmptySpaceWarpableCompanion[Scout] {
  override val maxHp = HP(25)
  override val warpTime = WarpTime(0)
  override val cost = Resources(3)
  override val populationCost = Population(1)
  override val movement = Movement.fromTiles(20)
  override val visibility = RectDistance(6)
  override val kind = WObjKind.Light
  override val InitialMovement = movement

  override def warp(owner: Player, position: Vect2) = Scout(position, owner)
}

case class ScoutOps(self: Scout) extends WUnitOps[Scout] {
  override protected def setMoveValues(
    position: Vect2, movementLeft: Movement
  ) = self.copy(position = position, movementLeft = movementLeft)
  override def setWarpState(newState: WarpTime) =
    self.copy(warpState = newState)
  override def withNewHp(hp: HP) = self.copy(hp = hp)
}