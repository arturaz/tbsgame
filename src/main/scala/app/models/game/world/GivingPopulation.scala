package app.models.game.world

import app.models.game.Population

trait GivingPopulationStats extends OwnedObjStats {
  val populationGiven: Population
}
