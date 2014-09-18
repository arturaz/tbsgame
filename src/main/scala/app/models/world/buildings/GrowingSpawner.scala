package app.models.world.buildings

import app.models.world.units.WUnit
import app.models.world._

trait GrowingSpawnerStats extends FactionObjStats {
  val DefaultTurnsPerStrength: Int
}

trait GrowingSpawner extends TurnCounter with FactionObj {
  type Self <: GrowingSpawner
  type Stats <: GrowingSpawnerStats
  type Controlled = WUnit with Fighter

  def strength = turns / turnsPerStrength
  val turnsPerStrength: Int
  def withTurnsPerStrength(value: Int): Self

  def spawn(position: Vect2): Controlled
}
