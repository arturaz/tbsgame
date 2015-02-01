package app.models.game.world.buildings

import app.models.game.{Actions, Player}
import app.models.game.world._
import implicits._

object LaserTower extends WBuildingCompanion[LaserTower]
with EmptySpaceWarpableCompanion[LaserTower]
with ReactiveFighterCompanion[LaserTower]
with SpecialActionCompanion[LaserTower]
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

  override def warp(owner: Player, position: Vect2) =
    LaserTower(position, owner)
  override def setWarpState(newState: WarpTime)(self: LaserTower) =
    self.copy(warpState=newState)
  override def withAttacksLeft(value: Attacks)(self: LaserTower) =
    self.copy(attacksLeft=value)
  override def withNewHp(hp: HP)(self: LaserTower) = self.copy(hp = hp)
  override def withNewXP(xp: XP)(self: LaserTower) = self.copy(xp = xp)
}

case class LaserTower(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=LaserTower.maxHp, xp: XP=LaserTower.InitialXP,
  warpState: WarpTime=LaserTower.InitialWarpState,
  attacksLeft: Attacks=LaserTower.InitialAttacks
) extends PlayerBuilding with WBuilding with ReactiveFighter with SpecialAction
{
  type Self = LaserTower
  def self = this
  override def companion = LaserTower
  override type Companion = LaserTower.type

  protected def specialImpl(world: World) =
    companion.withAttacksLeftEvt(companion.attacks)(world, self)
      .flatMap { newSelf => world.updated(self, newSelf)}
      .right
}