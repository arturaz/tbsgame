package app.models.game.events

import app.models.Attack
import app.models.world.{WObject, Vect2}

sealed trait Event

case class MoveEvt(
  obj: WObject.Id, to: Vect2, movesLeft: Int
) extends Event
case class AttackEvt(
  attacker: WObject.Id, defender: WObject.Id, attack: Attack
) extends Event
