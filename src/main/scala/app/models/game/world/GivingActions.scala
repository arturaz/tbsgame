package app.models.game.world

import app.models.game.Actions

trait GivingActionsOps[Self] extends OwnedObjOps[Self]

trait GivingActionsStats extends OwnedObjStats {
  val actionsGiven: Actions
}

trait GivingActionsCompanion[Self] extends GivingActionsOps[Self] with GivingActionsStats

trait GivingActions extends OwnedObj {
  type Companion <: GivingActionsOps[Self] with GivingActionsStats
}
