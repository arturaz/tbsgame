package app.models.world.buildings

import app.models.world._
import app.models.world.units.WUnit

trait GrowingSpawnerOps[Self <: GrowingSpawner]
extends TurnCounterOps[Self] with OwnedObjOps[Self] {
  def withTurnsPerStrength(value: Int)(self: Self): Self
}

trait GrowingSpawnerStats extends TurnCounterStats with OwnedObjStats {
  val DefaultTurnsPerStrength: Int
}

trait GrowingSpawnerCompanion[Self <: GrowingSpawner]
extends GrowingSpawnerOps[Self] with GrowingSpawnerStats

trait GrowingSpawner extends TurnCounter with BotObj {
  type Self <: GrowingSpawner
  type Companion <: GrowingSpawnerOps[Self] with GrowingSpawnerStats
  type Controlled = WUnit with Fighter

  def strength = turns / turnsPerStrength
  val turnsPerStrength: Int

  /* Try to spawn at position, returning Right(None) if unit was killed after spawn. */
  def spawn(
    world: World, position: Vect2
  ): Either[String, WObject.WorldObjOptUpdate[Controlled]]
}
