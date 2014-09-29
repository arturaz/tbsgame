package app.models.game.events

import app.algorithms.Pathfinding
import app.models.world._
import app.models.{Attack, Owner}

sealed trait Event

case class WarpEvt(obj: Warpable) extends Event

object MoveEvt {
  def apply(
    unit: MovableWObject, path: Pathfinding.Path
  ): Vector[MoveEvt] =
    path.vects.view.zipWithIndex.drop(1).take(unit.movementLeft.value).
    map { case (v, idx) =>
      MoveEvt(unit.id, v, unit.movementLeft - TileDistance(idx))
    }.to[Vector]
}
case class MoveEvt(
  obj: WObject.Id, to: Vect2, movesLeft: TileDistance
) extends Event

case class AttackEvt[D <: OwnedObj](
  attacker: Fighter, defender: (D, Option[D]), attack: Attack
) extends Event

case class MovementChangeEvt(
  obj: WObject.Id, movementLeft: TileDistance
) extends Event

case class ResourceChangeEvt(
  obj: Either[WObject.Id, Owner.Id], resourceDiff: Int
) extends Event

case class ActionChangeEvt(
  player: Owner.Id, actions: Int
) extends Event
