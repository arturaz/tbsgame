package app.models.game.world

import akka.event.LoggingAdapter
import app.algorithms.Pathfinding.Path
import app.models.game.events._
import implicits._
import utils.data.NonEmptyVector

trait MovableWObjectOps[Self <: MovableWObject]
extends WObjectOps with MoveAttackActionedOps[Self]
{ _: MovableWObjectStats =>
  protected def setMoveValues(position: Vect2, movementLeft: TileDistance)(self: Self): Self

  def setMoveValues(world: World, self: Self, newMovement: TileDistance): Evented[Self] = {
    val newSelf = self |> setMoveValues(self.position, newMovement)
    Evented(
      newSelf,
      if (self === newSelf) Vector.empty
      else Vector(MovementChangeEvt(world, newSelf))
    )
  }

  def resetMovementLeft(world: World, self: Self): Evented[Self] =
    setMoveValues(world, self, movement)

  def moveTo(world: World, target: Vect2)(self: Self): Evented[Self] = {
    for {
      newSelf <- withMovedOrAttackedEvt(true)(world, self)
      newSelf <- Evented(
        newSelf |>
        setMoveValues(target, newSelf.movementLeft - self.position.tileDistance(target))
      )
      newSelf <- Evented(newSelf, MoveEvt(world, self, target, newSelf.movementLeft))
    } yield newSelf
  }
}

trait MovableWObjectStats extends WObjectStats with MoveAttackActionedStats {
  val movement: TileDistance
}

trait MovableWObjectCompanion[Self <: MovableWObject]
extends MovableWObjectOps[Self] with MovableWObjectStats

/* Objects that can move. All such objects have 1x1 size. */
trait MovableWObject extends WObject with MoveAttackActioned
with Mobility[Mobility.Movable.type] { traitSelf =>
  type Self >: traitSelf.type <: MovableWObject
  type Companion <: MovableWObjectOps[Self] with MovableWObjectStats

  val movementLeft: TileDistance
  def movementZone = movementLeft.reachable(position)

  /* Returns a set of objects which are in units movement zone. */
  def obstacles(objects: Set[WObject]) =
    // TODO: fix the optimization
    objects/*.filter(o => movementZone.exists(o.bounds.contains))*/

  override def teamTurnStartedSelf(world: World)(implicit log: LoggingAdapter) =
    super.teamTurnStartedSelf(world) |>
    selfEventedUpdate(companion.resetMovementLeft) |>
    selfEventedUpdate(
      companion.withMovedOrAttackedEvt(companion.InitialMovedOrAttacked)
    )

  def moveTo(
    world: World, path: Path
  ): Either[String, WObject.WorldObjOptUpdate[Self]] = {
    if (path.movementNeeded > movementLeft)
      s"$this needed ${path.movementNeeded} movement for $path, had $movementLeft".left
    else if (path.vects.head =/= position)
      s"Starting $path vect =/= $this position".left
    else if (isWarpingIn)
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
    Path.validate(world, position, path).left.map(err =>
      s"Can't validate path: $position, $path: $err"
    ).right.flatMap(moveTo(world, _))

  private[this] def travel(
    vects: Seq[Vect2], current: WObject.WorldObjOptUpdate[Self]
  ): WObject.WorldObjOptUpdate[Self] = {
    if (vects.isEmpty) current
    else current.flatMap {
      case (world, Some(self)) => travel(
        vects.tail,
        for {
          newSelf <- self |> companion.moveTo(world, vects.head)
          movedWorld = world.updated(self, newSelf)
          revealedWorld <- World.revealObjects(owner.team, movedWorld)
          t <- revealedWorld.reactTo(newSelf)
        } yield t
      )
      case orig @ (_, None) => Evented(orig)
    }
  }


  def hasAttack(current: WObject.WorldObjOptUpdate[Self]) = current.events.exists(_.isInstanceOf[AttackEvt[_]])
}
