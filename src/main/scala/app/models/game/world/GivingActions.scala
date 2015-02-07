package app.models.game.world

import app.models.game.Actions

trait GivingActionsStats extends OwnedObjStats {
  val actionsGiven: Actions
}

trait GivingActionsImpl extends OwnedObjImpl {
  val stats: GivingActionsStats
}
