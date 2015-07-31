package app.models.game.world.units

import app.models.game.{Player, Population}
import app.models.game.world._

trait DroneStatsImpl extends EmptySpaceWarpableCompanion[Drone] { _: DroneStats.type =>
  override val maxHp = HP(35)
  override val movement = Movement.fromTiles(8)
  override val visibility = RectDistance(5)
  override val cost = Resources(1)
  override val populationCost = Population(1)
  override val kind = WObjKind.Light
  override val warpTime = WarpTime(0)

  override def warp(owner: Player, position: Vect2) = Drone(position, owner)
}

case class DroneOps(self: Drone) extends WUnitOps[Drone] {
  override def setWarpState(newState: WarpTime) =
    self.copy(warpState = newState)
  override def setMoveValues(position: Vect2, movementLeft: Movement) =
    self.copy(position = position, movementLeft = movementLeft)
  override def withNewHp(hp: HP) = self.copy(hp = hp)
}
