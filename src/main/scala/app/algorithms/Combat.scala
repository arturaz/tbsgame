package app.algorithms

import app.algorithms.Pathfinding.SearchRes
import app.models.game.events.Evented
import app.models.game.world._
import implicits._

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

    unit.moveTo(world, moveTarget).right.flatMap { movedEvtWorldSelf =>
      lazy val movedEvtWorld = movedEvtWorldSelf.map(_._1)
      movedEvtWorldSelf.value match {
        case (newWorld, Some(movedUnit)) =>
          movedUnit.attackW(target.value, newWorld).fold(
            err => if (! strict) movedEvtWorld.right else err.left,
            attackedEvtWorld => (movedEvtWorldSelf.events ++: attackedEvtWorld).right
          )
        case (_, None) => movedEvtWorld.right
      }
    }
  }

  /* Tries to move attack, but does not fail if cannot. */
  def moveAttackLoose(
    world: World, unit: MovableWObject with Fighter, target: SearchRes[OwnedObj]
  ): ActionResult = moveAttack(world, unit, target, strict = false).fold(
    err => throw new Exception(s"[search res=$target]: $err]"), identity
  )
}
