package app.models.game.world.buildings

import akka.event.LoggingAdapter
import app.models.game.{Actions, Player}
import app.models.game.world._
import implicits._
import app.models.game.world.Ops._

object LaserTowerStats extends WBuildingStats with SpecialActionStats with FighterStats
with EmptySpaceWarpableCompanion[LaserTower]
{
  override val maxHp = HP(550)
  override val attack = Atk(60)
  override val attacks = Attacks(3)
  override val warpTime = WarpTime(2)
  override val cost = Resources(12)
  override val attackRange = RadialDistance.Five
  override val visibility = RectDistance(4)
  override val kind = WObjKind.Heavy
  override val specialActionsNeeded = Actions(2)

  override def warp(owner: Player, position: Vect2) = LaserTower(position, owner)
}

case class LaserTowerOps(self: LaserTower) extends ReactiveFighterOps[LaserTower]
with FighterOps[LaserTower] with WarpableOps[LaserTower]
{
  override def setWarpState(newState: WarpTime) = self.copy(warpState=newState)
  override def withAttacksLeft(value: Attacks) = self.copy(attacksLeft=value)
  override def withNewHp(hp: HP) = self.copy(hp = hp)
  override def withNewXP(xp: XP) = self.copy(xp = xp)
}

trait LaserTowerImpl { _: LaserTower =>
  protected def specialImpl
  (world: World, invokedBy: Player)(implicit log: LoggingAdapter) =
    toFighterOps(this).withAttacksLeftEvt(stats.attacks)(world)
      .flatMap { newSelf => world.updated(this, newSelf) }
      .right
}