package app.models.game.world.units

import akka.event.LoggingAdapter
import app.models.game.world._
import app.models.game.{Population, Actions, Player}
import app.models.game.world.Ops._
import implicits._

trait WarpPrismStatsImpl extends EmptySpaceWarpableCompanion[WarpPrism]
{ _: WarpPrismStats.type =>
  override val maxHp = HP(250)
  override val movement = Movement.fromTiles(6)
  override val visibility = RectDistance(4)
  override val cost = Resources(10)
  override val populationCost = Population(3)
  override val kind = WObjKind.Light

  override val specialActionsNeeded = Actions(1)

  override def warp(owner: Player, position: Vect2) = WarpPrism(position, owner)
}

case class WarpPrismOps(self: WarpPrism) extends WUnitOps[WarpPrism]
{
  override def setWarpState(newState: WarpTime) = self.copy(warpState = newState)
  override def setMoveValues(position: Vect2, movementLeft: Movement) =
    self.copy(position = position, movementLeft = movementLeft)
  override def withNewHp(hp: HP) = self.copy(hp = hp)
}

trait WarpPrismImpl { _: WarpPrism =>
  type Stats = WarpPrismStats.type
  override val stats = WarpPrismStats

  override def specialImpl
  (world: World, invokedBy: Player)(implicit log: LoggingAdapter) = {
    val evtEither = for {
      world <- world remove this
      eitherWorld <- WarpLinkerStats.warpW(
        world, owner, position, checkVisibility = false
      ).extract
    } yield eitherWorld
    val res = evtEither.extract
    res
  }
}
