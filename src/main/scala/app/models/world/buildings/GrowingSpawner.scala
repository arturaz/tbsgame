package app.models.world.buildings

import app.models.world.units.WUnit
import app.models.world._

trait GrowingSpawnerOps[Self <: GrowingSpawner]
extends TurnCounterOps[Self] with OwnedObjOps[Self] {
  def withTurnsPerStrength(value: Int)(self: Self): Self
}

trait GrowingSpawnerStats extends TurnCounterStats with OwnedObjStats {
  val DefaultTurnsPerStrength: Int
}

trait GrowingSpawnerCompanion[Self <: GrowingSpawner]
extends GrowingSpawnerOps[Self] with GrowingSpawnerStats

trait GrowingSpawner extends TurnCounter with OwnedObj {
  type Self <: GrowingSpawner
  type Companion <: GrowingSpawnerOps[Self] with GrowingSpawnerStats
  type Controlled = WUnit with Fighter

  def strength = turns / turnsPerStrength
  val turnsPerStrength: Int

  def spawn(position: Vect2): Controlled
}
