package app.models.game.world

import app.models.game.Actions

trait GivingActionsOps[Self <: GivingActions] extends OwnedObjOps[Self]

trait GivingActionsStats extends OwnedObjStats {
  val actionsGiven: Actions
}

trait GivingActionsCompanion[Self <: GivingActions] extends GivingActionsOps[Self]
with GivingActionsStats

trait GivingActions extends OwnedObj {
  type Self <: GivingActions
  type Companion <: GivingActionsOps[Self] with GivingActionsStats
}
