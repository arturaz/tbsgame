package app.models.game.world.units

import akka.event.LoggingAdapter
import app.models.game.events.{AttacksChangedEvt, MovementChangeEvt, StatsChangedEvt}
import app.models.game.{Actions, Population, Player}
import app.models.game.world._
import scalaz._, Scalaz._

trait RocketFrigateCommonStatsImpl extends EmptySpaceWarpableCompanion[RocketFrigate] {
_: RocketFrigateCommonStats =>
  override val maxHp = HP(30)
  override val attack = Atk(210)
  override val attackOverrides = Map(WObjKind.light -> Atk(95))
  override val attacks = Attacks(1)
  override val cost = Resources(8)
  override val populationCost = Population(3)
  override val kind = WObjKind.Medium
  override val specialActionsNeeded = Actions(1)
  val deployed: Boolean

  override def warp(owner: Player, position: Vect2) = RocketFrigate(position, owner)
}

trait RocketFrigateStatsImpl extends RocketFrigateCommonStatsImpl
{ _: RocketFrigateStats.type =>
  override val attackRange = RadialDistance.Two
  override val movement = Movement.fromTiles(8)
  override val visibility = RectDistance(2)
  override val deployed = false
}

trait RocketFrigateDeployedStatsImpl extends RocketFrigateCommonStatsImpl
{ _: RocketFrigateDeployedStats.type =>
  override val attackRange = RadialDistance.Six
  override val movement = Movement.fromTiles(0)
  override val visibility = RectDistance(4)
  override val deployed = true
}

trait RocketFrigateImpl { _: RocketFrigate =>
  override protected def specialImpl(
    world: World, invokedBy: Player
  )(implicit log: LoggingAdapter) = {
    val newSelf = copy(
      stats = if (stats.deployed) RocketFrigateStats else RocketFrigateDeployedStats,
      movementLeft = Movement.zero, attacksLeft = Attacks(0)
    )
    val evented = Vector(
      StatsChangedEvt(world.visibilityMap, newSelf),
      MovementChangeEvt(world.visibilityMap, newSelf),
      AttacksChangedEvt(world.visibilityMap, newSelf)
    ) ++: world.updated(this, newSelf)
    evented.right
  }
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