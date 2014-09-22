package app.models.world

import app.models.game.events.Event

trait SpecialActionStats extends WObjectStats {
  val specialActionsNeeded: Int
}

trait SpecialAction extends WObject {
  type Self <: SpecialAction
  type Stats <: SpecialActionStats

  def special(world: World): Either[String, (World, Vector[Event])]
}
