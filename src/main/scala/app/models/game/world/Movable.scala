package app.models.game.world

import akka.event.LoggingAdapter
import app.algorithms.Pathfinding.Path
import app.models.game.events._
import app.models.game.world.units._
import implicits._
import utils.data.NonEmptyVector
import Ops._

import scala.language.implicitConversions

trait MovableStatsImpl { _: MovableStats =>
  val movement: Movement
  val InitialMovement = Movement.fromTiles(0)
}

trait MovableImpl extends OwnedObjImpl with MobilityMovable {
  type Stats <: MovableStats

  val movementLeft: Movement
}

trait ToMovableOps {
  implicit def toMovableOps[A <: Movable](a: A): MovableOps[A] = (((a: Movable) match {
    case o: Corvette => CorvetteOps(o)
    case o: Fortress => FortressOps(o)
    case o: Gunship => GunshipOps(o)
    case o: RayShip => RayShipOps(o)
    case o: RocketFrigate => RocketFrigateOps(o)
    case o: Scout => ScoutOps(o)
    case o: Wasp => WaspOps(o)
  }): MovableOps[_]).asInstanceOf[MovableOps[A]]
}

trait MovableOps[Self <: Movable] extends OwnedObjOps[Self] {
  def self: Self

  def moveTo(
    world: World, path: Path
  )(implicit log: LoggingAdapter): Either[String, WObject.WorldObjOptUpdate[Self]] = {
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
  )(implicit log: LoggingAdapter): Either[String, WObject.WorldObjOptUpdate[Self]] =
    Path.validate(world, self.position, path).left.map(err =>
      s"Can't validate path: ${self.position}, $path: $err"
    ).right.flatMap(moveTo(world, _))

  private[this] def travel(
    vects: Seq[Vect2], current: WObject.WorldObjOptUpdate[Self]
  )(implicit log: LoggingAdapter): WObject.WorldObjOptUpdate[Self] = {
    if (vects.isEmpty) current
    else current.flatMap {
      case (world, Some(self)) => travel(
        vects.tail,
        for {
          newSelf <- self.moveTo(world, vects.head)
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
      else Vector(MovementChangeEvt(world.visibilityMap, newSelf))
    )
  }

  def resetMovementLeft(world: World): Evented[Self] =
    setMoveValues(world, self.stats.movement)

  def moveTo(world: World, target: Vect2): Evented[Self] = {
    for {
      newSelf <- Evented(
        setMoveValues(target, self.movementLeft - self.position.movementDistance(target))
      )
      newSelf <- Evented(
        newSelf,
        MoveEvt(world.visibilityMap, self, target, newSelf.movementLeft)
      )
    } yield newSelf
  }

  final def movableTeamTurnStarted(world: World)(implicit log: LoggingAdapter) =
    WObject.selfEventedUpdate(world, self, resetMovementLeft(world))
}