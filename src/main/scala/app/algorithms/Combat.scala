package app.algorithms

import app.algorithms.Pathfinding.SearchRes
import app.models.game.events.{AttackEvt, Event, MoveEvt}
import app.models.world.{OwnedObj, Fighter, MovableWObject, World}

object Combat {
  type ActionResult = (World, Vector[Event])
  type EitherActionResult = Either[String, ActionResult]

  def moveAttack(
    world: World, unit: MovableWObject with Fighter,
    target: SearchRes[OwnedObj], strict: Boolean=true
  ): EitherActionResult = {
    val moveTarget =
      if (!strict && unit.movementLeft < target.movementNeeded)
        target.path.limit(unit.movementLeft)
      else
        target.path

    unit.moveTo(moveTarget.vects.last).right.flatMap { movedUnit =>
      val moves = MoveEvt(unit, moveTarget)

      movedUnit.attack(target.value).fold(
        err =>
          if (! strict) Right((world.update(unit, movedUnit), moves))
          else Left(err),
        { case (attack, attackUnit) =>
          Right((
            world.update(unit, attackUnit).update(attack, target.value),
            moves :+ AttackEvt(unit.id, target.value.id, attack)
          ))
        }
      )
    }
  }

  /* Tries to move attack, but does not fail if cannot. */
  def moveAttackLoose(
    world: World, unit: MovableWObject with Fighter, target: SearchRes[OwnedObj]
  ): ActionResult = moveAttack(world, unit, target, strict = false).fold(
    err => throw new Exception(s"[search res=$target]: $err]"), identity
  )
}
