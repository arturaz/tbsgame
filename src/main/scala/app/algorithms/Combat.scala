package app.algorithms

import app.algorithms.Pathfinding.{Path, SearchRes}
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
    moveAndThen(world, unit, target.path, strict) { case (newWorld, movedUnit) =>
      movedUnit.attackW(target.value, newWorld)
    }
  }

  def moveAndThen(
    world: World, unit: MovableWObject with Fighter,
    movePath: Path, strict: Boolean=true
  )(
    andThen: (World, MovableWObject with Fighter) => EitherActionResult
  ): EitherActionResult = {
    val moveTarget =
      if (!strict && unit.movementLeft < movePath.movementNeeded)
        movePath.limit(unit.movementLeft)
      else
        movePath

    unit.moveTo(world, moveTarget).right.flatMap { movedEvtWorldSelf =>
      @inline def movedEvtWorld = movedEvtWorldSelf.map(_._1)
      movedEvtWorldSelf.value match {
        case (newWorld, Some(movedUnit)) => andThen(
          newWorld, movedUnit.asInstanceOf[MovableWObject with Fighter]
        ).fold(
          err => if (! strict) movedEvtWorld.right else err.left,
          andThenEvtWorld => (movedEvtWorldSelf.events ++: andThenEvtWorld).right
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
