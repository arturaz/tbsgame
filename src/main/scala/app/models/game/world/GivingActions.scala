package app.models.game.world

import app.models.game.Actions

trait GivingActionsStatsImpl { _: GivingActionsStats =>
  val actionsGiven: Actions
}

trait GivingActionsImpl extends OwnedObjImpl {
  type Stats <: GivingActionsStats
}
