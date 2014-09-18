package app.models.game.events

import app.algorithms.Pathfinding
import app.models.Attack
import app.models.world.{MovableWObject, TileDistance, Vect2, WObject}

sealed trait Event

case class SpawnEvt(obj: WObject) extends Event

object MoveEvt {
  def fromPath(
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
case class AttackEvt(
  attacker: WObject.Id, defender: WObject.Id, attack: Attack
) extends Event
