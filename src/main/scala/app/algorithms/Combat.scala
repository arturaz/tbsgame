package app.algorithms

import app.algorithms.Pathfinding.{Path, SearchRes}
import app.models.game.events.Evented
import app.models.game.world._
import implicits._
import utils.ErrOpt

object Combat {
  type RawResult[A <: MovableFighter] = Evented[(World, Option[A])]
  type RawWorldResult = Evented[World]
  type Result[A <: MovableFighter] = ErrOpt[RawResult[A]]
  type WorldResult = ErrOpt[RawWorldResult]
  type MovableFighter = Movable with Fighter

  def moveAttack[A <: MovableFighter](
    world: World, unit: A,
    target: SearchRes[OwnedObj], strict: Boolean=true
  ): Result[A] = {
    moveAndThen(world, unit, target.path, strict) { case (newWorld, movedUnit) =>
      movedUnit.attackWS(target.value, newWorld)
    }
  }

  def moveAndThen[A <: MovableFighter](
    world: World, unit: A,
    movePath: Path, strict: Boolean=true
  )(
    andThen: (World, A) => ErrOpt[Evented[(World, A)]]
  ): Result[A] = {
    val moveTarget =
      if (!strict && unit.movementLeft < movePath.movementNeeded)
        movePath.limit(unit.movementLeft)
      else
        movePath

    unit.moveTo(world, moveTarget).right.flatMap { movedEvtWorldSelf =>
      movedEvtWorldSelf.value match {
        case (newWorld, Some(movedUnit)) => andThen(newWorld, movedUnit).fold(
          err => if (! strict) movedEvtWorldSelf.right else err.left,
          andThenEvtWorld => (
            movedEvtWorldSelf.events ++:
              andThenEvtWorld.map { case (w, u) => (w, Some(u)) }
          ).right
        )
        case (_, None) => movedEvtWorldSelf.right
      }
    }
  }

  /* Tries to move attack, but does not fail if cannot. */
  def moveAttackLoose[A <: MovableFighter](
    world: World, unit: A, target: SearchRes[OwnedObj]
  ): RawResult[A] =
    moveAttack(world, unit, target, strict = false).fold(
      err => throw new Exception(s"[search res=$target]: $err]"), identity
    )
}
