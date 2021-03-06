package app.models.game.world.units

import akka.event.LoggingAdapter
import app.models.game.world._
import app.models.game.{Population, Actions, Player}
import app.models.game.world.Ops._

import implicits._
import scalaz._, Scalaz._

trait CorvetteStatsImpl extends EmptySpaceWarpableCompanion[Corvette]
{ _: CorvetteStats.type =>
  override val maxHp = HP(120)
  override val attack = Atk(68)
  override val attacks = Attacks(2)
  override val attackRange = RadialDistance.Four
  override val movement = Movement.fromTiles(10)
  override val visibility = RectDistance(4)
  override val cost = Resources(6)
  override val populationCost = Population(2)
  override val kind = WObjKind.Light

  val specialMovementAdded = Movement.fromTiles(8)
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

  override def specialImpl
  (world: World, invokedBy: Player)(implicit log: LoggingAdapter) =
    if (noAttacksLeft) s"Cannot toggle special action for $this: already attacked!".left
    else {
      val evtWorld = for {
        self <- toFighterOps(this).withAttacksLeftEvt(attacksLeft - Attacks(1))(world)
        self <- toMovableOps(self).setMoveValues(
          world, movementLeft + stats.specialMovementAdded
        )
        world <- world.updated(this, self)
      } yield world
      evtWorld.right
    }
}
