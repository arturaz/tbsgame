package app.models.game.world.units

import app.models.game.world._
import app.models.game.{Population, Actions, Player}
import app.models.game.world.Ops._

object CorvetteStats extends WFighterUnitStats with SpecialActionStats
with EmptySpaceWarpableCompanion[Corvette]
{
  override val maxHp = HP(120)
  override val attack = Atk(45)
  override val attacks = Attacks(3)
  override val attackRange = RadialDistance.Five
  override val movement = Movement.fromTiles(12)
  override val visibility = RectDistance(4)
  override val warpTime = WarpTime(0)
  override val cost = Resources(6)
  override val populationCost = Population(2)
  override val kind = WObjKind.Medium

  val specialMovementAdded = Movement.fromTiles(6)
  override val specialActionsNeeded = Actions(0)

  override def warp(owner: Player, position: Vect2) = Corvette(position, owner)
}

case class CorvetteOps(self: Corvette) extends WFighterUnitOps[Corvette]
{
  override def setWarpState(newState: WarpTime) = self.copy(warpState = newState)
  override def setMoveValues(position: Vect2, movementLeft: Movement) =
    self.copy(position = position, movementLeft = movementLeft)
  override def withNewHp(hp: HP) = self.copy(hp = hp)
  override def withNewXP(xp: XP) = self.copy(xp = xp)
  override def withAttacksLeft(value: Attacks) = self.copy(attacksLeft = value)
}

trait CorvetteImpl { _: Corvette =>
  type Stats = CorvetteStats.type
  override val stats = CorvetteStats

  override def specialImpl(world: World, invokedBy: Player) = Either.cond(
    ! noAttacksLeft,
    {
      for {
        self <- toFighterOps(this).withAttacksLeftEvt(attacksLeft - Attacks(1))(world)
        self <- toMovableOps(self).setMoveValues(
          world, movementLeft + stats.specialMovementAdded
        )
        world <- world.updated(this, self)
      } yield world
    },
    s"Cannot toggle special action for $this: already attacked!"
  )
}
