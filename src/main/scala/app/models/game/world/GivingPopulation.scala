package app.models.game.world

import app.models.game.Population

trait GivingPopulationStatsImpl { _: GivingPopulationStats =>
  val populationGiven: Population
}

trait GivingPopulationImpl extends OwnedObjImpl {
  type Stats <: GivingPopulationStats

  def populationGiven = if (isWarpedIn) stats.populationGiven else Population(0)
}
