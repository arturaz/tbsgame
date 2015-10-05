package app.algorithms

import akka.event.LoggingAdapter
import app.algorithms.Pathfinding.{Path, SearchRes}
import app.models.game.events.Evented
import app.models.game.world._
import app.models.game.world.Ops._
import implicits._
import scalaz._, Scalaz._
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
  )(implicit log: LoggingAdapter): Result[A] = {
    moveAndThen(world, unit, target.path, strict) { case (newWorld, movedUnit) =>
      movedUnit.attackWS(target.value, newWorld)
    }
  }

  def moveAndThen[A <: MovableFighter](
    world: World, unit: A,
    movePath: Path, strict: Boolean=true
  )(
    andThen: (World, A) => ErrOpt[Evented[(World, Option[A])]]
  )(implicit log: LoggingAdapter): Result[A] = {
    val moveTarget =
      if (!strict && unit.movementLeft < movePath.movementNeeded)
        movePath.limit(unit.movementLeft)
      else
        movePath

    unit.moveTo(world, moveTarget).flatMap { movedEvtWorldSelf =>
      movedEvtWorldSelf.value match {
        case (newWorld, Some(movedUnit)) => andThen(newWorld, movedUnit).fold(
          err => if (! strict) movedEvtWorldSelf.right else err.left,
          andThenEvtWorld => (movedEvtWorldSelf.events ++: andThenEvtWorld).right
        )
        case (_, None) => movedEvtWorldSelf.right
      }
    }
  }

  /* Tries to move attack, but does not fail if cannot. */
  def moveAttackLoose[A <: MovableFighter](
    world: World, unit: A, target: SearchRes[OwnedObj]
  )(implicit log: LoggingAdapter): RawResult[A] =
    moveAttack(world, unit, target, strict = false).fold(
      err => throw new Exception(s"[search res=$target]: $err]"), identity
    )
}
