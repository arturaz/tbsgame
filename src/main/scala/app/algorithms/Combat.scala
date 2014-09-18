package app.algorithms

import app.algorithms.Pathfinding.SearchRes
import app.models.game.events.{AttackEvt, Event, MoveEvt}
import app.models.world.{FactionObj, Fighter, MovableWObject, World}

object Combat {
  type ActionResult = (World, Vector[Event])
  type EitherActionResult = Either[String, ActionResult]

  def moveAttack(
    world: World, unit: MovableWObject with Fighter,
    target: SearchRes[FactionObj], strict: Boolean=true
  ): EitherActionResult = {
    val moveTarget =
      if (!strict && unit.movementLeft < target.movementNeeded)
        target.path.limit(unit.movementLeft)
      else
        target.path

    unit.moveTo(moveTarget.vects.last).right.flatMap { movedUnit =>
      val moves = MoveEvt.fromPath(unit, moveTarget)

      movedUnit.attack(target.value).fold(
        err =>
          if (! strict && moveTarget != target.path)
            Right((world.update(unit, movedUnit), moves))
          else
            Left(err),
        { case (attack, attackUnit) =>
          Right((
            world.update(unit, attackUnit).update(attack, target.value),
            moves :+ AttackEvt(unit.id, target.value.id, attack)
          ))
        }
      )
    }
  }

  def moveAttackLoose(
    world: World, unit: MovableWObject with Fighter, target: SearchRes[FactionObj]
  ): ActionResult = moveAttack(world, unit, target).right.get
}
