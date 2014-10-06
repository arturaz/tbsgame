package app.models.world

import app.algorithms.Pathfinding
import app.algorithms.Pathfinding.Path
import app.models.game.events.Evented
import implicits._

trait MovableWObjectOps[Self <: MovableWObject]
extends WObjectOps with MoveAttackActionedOps[Self]
{ _: MovableWObjectStats =>
  def setMoveValues(position: Vect2, movementLeft: TileDistance)(self: Self): Self
  def resetMovementLeft(self: Self) =
    self |> setMoveValues(self.position, movement)

  def moveTo(target: Vect2)(self: Self): Self = {
    self |>
      setMoveValues(target, self.movementLeft - self.position.tileDistance(target)) |>
      withMovedOrAttacked(true)
  }
}

trait MovableWObjectStats extends WObjectStats with MoveAttackActionedStats {
  val movement: TileDistance
}

trait MovableWObjectCompanion[Self <: MovableWObject]
extends MovableWObjectOps[Self] with MovableWObjectStats

/* Objects that can move. All such objects have 1x1 size. */
trait MovableWObject extends WObject with MoveAttackActioned
with Mobility[Mobility.Movable.type] {
  type Self <: MovableWObject
  type Companion <: MovableWObjectOps[Self] with MovableWObjectStats

  val movementLeft: TileDistance
  def movementZone = movementLeft.reachable(position)

  /* Returns a set of objects which are in units movement zone. */
  def obstacles(objects: Set[WObject]) =
    // TODO: fix the optimization
    objects/*.filter(o => movementZone.exists(o.bounds.contains))*/

  override def teamTurnStartedSelf(world: World) =
    super.teamTurnStartedSelf(world) |>
    selfUpdate(companion.resetMovementLeft) |>
    selfUpdate(_ |> companion.withMovedOrAttacked(companion.InitialMovedOrAttacked))

  def moveTo(
    world: World, path: Path
  ): Either[String, WObject.WorldObjOptUpdate[Self]] = {
    if (path.movementNeeded > movementLeft)
      s"$this needed ${path.movementNeeded} movement for $path, had $movementLeft".left
    else if (path.vects.head != position)
      s"Starting $path vect != $this position".left
    else
      travel(path.vects, Evented((world, Some(self)))).right
  }

  def moveTo(
    world: World, target: Vect2
  ): Either[String, WObject.WorldObjOptUpdate[Self]] = {
    Pathfinding.aStar(
      self, target.toBounds, world.bounds, obstacles(world.objects).map(_.bounds)
    ).fold2(
      s"Can't find path from $position to $target for $self".left,
      moveTo(world, _)
    )
  }

  private[this] def travel(
    vects: Seq[Vect2], current: WObject.WorldObjOptUpdate[Self]
  ): WObject.WorldObjOptUpdate[Self] = {
    if (vects.isEmpty) current
    else current.flatMap {
      case (world, Some(self)) => travel(
        vects.tail,
        {
          val newSelf = self |> companion.moveTo(vects.head)
          val newEvtWorld = world.updated(self, newSelf)
          World.revealObjects(owner.team, newEvtWorld).flatMap(_.reactTo(newSelf))
        }
      )
      case (_, None) => current
    }
  }
}
