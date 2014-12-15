package app.models.game.world.buildings

import app.models.game.Actions
import app.models.game.world._
import app.models.game.world.units.WUnit

trait GrowingSpawnerOps[Self <: GrowingSpawner]
extends TurnCounterOps[Self] with OwnedObjOps[Self] {
  def withTurnsPerStrength(value: SpawnerStr)(self: Self): Self
}

trait GrowingSpawnerStats extends TurnCounterStats with OwnedObjStats {
  val DefaultStartingStrength: SpawnerStr
  /* Each action from buildings that the team controls reduces spawner strength by this
     much. This makes spawner get angrier as VPTowers are taken from it. */
  val StrengthReductionPerAction: SpawnerStr
  val DefaultTurnsPerStrength: SpawnerStr
}

trait GrowingSpawnerCompanion[Self <: GrowingSpawner]
extends GrowingSpawnerOps[Self] with GrowingSpawnerStats

trait GrowingSpawner extends TurnCounter with BotObj {
  type Self <: GrowingSpawner
  type Companion <: GrowingSpawnerOps[Self] with GrowingSpawnerStats
  type Controlled = WUnit with Fighter

  def strength: SpawnerStr = startingStrength + SpawnerStr(turns) / turnsPerStrength
  def strength(actions: Actions): SpawnerStr =
    strength - companion.StrengthReductionPerAction * SpawnerStr(actions.value)
  val startingStrength: SpawnerStr
  val turnsPerStrength: SpawnerStr

  /* Try to spawn at position, returning Right(None) if unit was killed after spawn. */
  def spawn(
    world: World, position: Vect2
  ): Either[String, WObject.WorldObjOptUpdate[Controlled]]
}
