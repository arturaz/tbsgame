package app.models.game.world

import app.models.game.Population

trait GivingPopulationOps[Self <: GivingPopulation] extends OwnedObjOps[Self]

trait GivingPopulationStats extends OwnedObjStats {
  val populationGiven: Population
}

trait GivingPopulationCompanion[Self <: GivingPopulation] extends GivingPopulationOps[Self]
with GivingPopulationStats

trait GivingPopulation extends OwnedObj {
  type Self <: GivingPopulation
  type Companion <: GivingPopulationOps[Self] with GivingPopulationStats
}
