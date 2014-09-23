package app.models.world

trait GivingActionsOps[Self] extends OwnedObjOps[Self]

trait GivingActionsStats extends OwnedObjStats {
  val actionsGiven: Int
}

trait GivingActionsCompanion[Self] extends GivingActionsOps[Self] with GivingActionsStats

trait GivingActions extends OwnedObj {
  type Companion <: GivingActionsOps[Self] with GivingActionsStats
}
