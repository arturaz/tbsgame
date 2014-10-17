package app.models.game.world

import app.models.game.Actions
import app.models.game.events.Evented

trait SpecialActionOps extends WObjectOps

trait SpecialActionStats extends WObjectStats {
  val specialActionsNeeded: Actions
}

trait SpecialActionCompanion extends SpecialActionOps with SpecialActionStats

trait SpecialAction extends WObject {
  type Self <: SpecialAction
  type Companion <: SpecialActionOps with SpecialActionStats

  def special(world: World): Either[String, Evented[World]]
}
