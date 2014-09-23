package app.algorithms

import app.algorithms.Pathfinding.SearchRes
import app.models.game.events.{AttackEvt, Evented, MoveEvt}
import app.models.world.{Fighter, MovableWObject, OwnedObj, World}

object Combat {
  type ActionResult = Evented[World]
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
          if (! strict) Right(Evented(world.updated(unit, movedUnit), moves))
          else Left(err),
        { case (attack, attackUnit) =>
          Right(Evented(
            world.updated(unit, attackUnit).update(attack, target.value),
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
