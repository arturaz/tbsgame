package app.models.game.world.buildings

import app.models.game.{Actions, Player}
import app.models.game.world._
import implicits._

object LaserTowerStats extends WBuildingStats with SpecialActionStats with FighterStats
with EmptySpaceWarpableCompanion[LaserTower]
{
  override val maxHp = HP(350)
  override val attack = Atk(60)
  override val attacks = Attacks(4)
  override val warpTime = WarpTime(1)
  override val cost = Resources(12)
  override val attackRange = RadialDistance(7.5f)
  override val visibility = RectDistance(7)
  override val kind = WObjKind.Heavy
  override val specialActionsNeeded = Actions(2)
  override def InitialAttacks = Attacks(0)

  override def warp(owner: Player, position: Vect2) = LaserTower(position, owner)
}

case class LaserTowerOps(self: LaserTower) extends FighterOps[LaserTower]
with WarpableOps[LaserTower]
{
  override def setWarpState(newState: WarpTime) = self.copy(warpState=newState)
  override def withAttacksLeft(value: Attacks) = self.copy(attacksLeft=value)
  override def withNewHp(hp: HP) = self.copy(hp = hp)
  override def withNewXP(xp: XP) = self.copy(xp = xp)
}

trait LaserTowerImpl { _: LaserTower =>
  protected def specialImpl(world: World) =
    toFighterOps(this).withAttacksLeftEvt(stats.attacks)(world)
      .flatMap { newSelf => world.updated(this, newSelf) }
      .right
}