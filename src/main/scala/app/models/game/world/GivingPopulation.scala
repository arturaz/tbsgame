package app.models.game.world

import app.models.game.Population

trait GivingPopulationStats extends OwnedObjStats {
  val populationGiven: Population
}

trait GivingPopulationImpl extends OwnedObjImpl {
  type Stats <: GivingPopulationStats

  def populationGiven = if (isWarpedIn) stats.populationGiven else Population(0)
}
