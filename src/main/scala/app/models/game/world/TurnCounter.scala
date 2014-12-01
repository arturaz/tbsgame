package app.models.game.world

import akka.event.LoggingAdapter
import implicits._

trait TurnCounterOps[Self <: TurnCounter] extends WObjectOps {
  def withTurns(turns: Int)(self: Self): Self
  def incTurns(self: Self) = withTurns(self.turns + 1)(self)
}

trait TurnCounterStats extends WObjectStats

trait TurnCounterCompanion[Self <: TurnCounter]
extends TurnCounterOps[Self] with TurnCounterStats

trait TurnCounter extends WObject {
  type Self <: TurnCounter
  type Companion <: TurnCounterOps[Self] with TurnCounterStats
  val turns: Int

  override def gameTurnStartedSelf(world: World)(implicit log: LoggingAdapter) =
    super.gameTurnStartedSelf(world) |>
    selfUpdate(companion.incTurns)
}
