package app.models.game.world

import akka.event.LoggingAdapter
import app.algorithms.Pathfinding.Path
import app.models.game.events._
import implicits._
import utils.data.NonEmptyVector

import scala.language.implicitConversions

trait MovableStats extends OwnedObjStats {
  val movement: Movement
}

trait MovableImpl extends OwnedObjImpl with Mobility[Mobility.Movable.type] {
  val movementLeft: Movement
  val stats: MovableStats
}

trait ToMovableOps {
  implicit def toMovableOps[A <: Movable](a: A): MovableOps[A] = (a match {

  }).asInstanceOf[MovableOps[A]]
}

object Movable extends ToMovableOps

trait MovableOps[+Self <: Movable] extends Any {
  def self: Self

  def moveTo(
    world: World, path: Path
  ): Either[String, WObject.WorldObjOptUpdate[Self]] = {
    if (path.movementNeeded > self.movementLeft)
      s"$this needed ${path.movementNeeded} movement for $path, had ${self.movementLeft
      }".left
    else if (path.vects.head =/= self.position)
      s"Starting $path vect =/= $this position".left
    else if (self.isWarpingIn)
      s"$this is still warping in!".left
    else if (path.movementNeeded.isZero)
      // If we don't need to go anywhere, don't go.
      Evented((world, Some(self))).right
    else
      // The first vect is self position
      travel(path.vects.tail, Evented((world, Some(self)))).right
  }

  def moveTo(
    world: World, path: NonEmptyVector[Vect2]
  ): Either[String, WObject.WorldObjOptUpdate[Self]] =
    Path.validate(world, self.position, path).left.map(err =>
      s"Can't validate path: ${self.position}, $path: $err"
    ).right.flatMap(moveTo(world, _))

  private[this] def travel(
    vects: Seq[Vect2], current: WObject.WorldObjOptUpdate[Self]
  ): WObject.WorldObjOptUpdate[Self] = {
    if (vects.isEmpty) current
    else current.flatMap {
      case (world, Some(self)) => travel(
        vects.tail,
        for {
          newSelf <- moveTo(world, vects.head)
          movedWorld = world.updated(self, newSelf)
          revealedWorld <- World.revealObjects(self.owner.team, movedWorld)
          t <- revealedWorld.reactTo(newSelf)
        } yield t
      )
      case orig @ (_, None) => Evented(orig)
    }
  }

  protected def setMoveValues(position: Vect2, movementLeft: Movement): Self

  def setMoveValues(world: World, newMovement: Movement): Evented[Self] = {
    val newSelf = setMoveValues(self.position, newMovement)
    Evented(
      newSelf,
      if (self === newSelf) Vector.empty
      else Vector(MovementChangeEvt(world, newSelf))
    )
  }

  def resetMovementLeft(world: World): Evented[Self] =
    setMoveValues(world, self.stats.movement)

  def moveTo(world: World, target: Vect2): Evented[Self] = {
    for {
      newSelf <- Evented(
        setMoveValues(target, self.movementLeft - self.position.movementDistance(target))
      )
      newSelf <- Evented(newSelf, MoveEvt(world, self, target, newSelf.movementLeft))
    } yield newSelf
  }

  def teamTurnFinishedSelf(world: World)(implicit log: LoggingAdapter) =
    WObject.selfEventedUpdate(world, self, resetMovementLeft(world))
}