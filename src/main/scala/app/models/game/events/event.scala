package app.models.game.events

import app.models.game._
import app.models.game.world._
import implicits._

/* Event that cannot be viewed anymore. */
sealed trait FinalEvent

/* Base event class. */
sealed trait Event extends FinalEvent {
  /* Some events expand into several events when viewed in prism of some owner. */
  def asViewedBy(owner: Owner): Iterable[FinalEvent]
}

sealed trait VisibleEvent extends Event {
  def asViewedBy(owner: Owner) =
    if (visibleBy(owner)) Iterable(this) else Iterable.empty
  def visibleBy(owner: Owner): Boolean
}

sealed trait AlwaysVisibleEvent extends VisibleEvent {
  override def visibleBy(owner: Owner) = true
}

sealed trait BoundedEvent extends VisibleEvent {
  def world: World
  def bounds: Bounds
  override def visibleBy(owner: Owner) = world.isVisiblePartial(owner, bounds)
}

case class HumanState(resources: Resources, gameState: GameHumanState)

case class JoinEvt(human: Human, state: HumanState) extends AlwaysVisibleEvent
case class LeaveEvt(human: Human) extends AlwaysVisibleEvent
case class TurnStartedEvt(team: Team) extends AlwaysVisibleEvent
case class TurnEndedEvt(team: Team) extends AlwaysVisibleEvent

sealed trait PointOwnershipChangeEvt extends VisibleEvent {
  def team: Team
  def ownedVects: Vector[Vect2]
  def unownedVects: Vector[Vect2]
  override def visibleBy(owner: Owner) = owner.team === team
}

case class WarpZoneChangeEvt(
  team: Team, ownedVects: Vector[Vect2]=Vector.empty,
  unownedVects: Vector[Vect2]=Vector.empty
) extends PointOwnershipChangeEvt

case class VisibilityChangeEvt(
  team: Team, visible: Vector[Vect2]=Vector.empty,
  invisible: Vector[Vect2]=Vector.empty
) extends PointOwnershipChangeEvt {
  override def ownedVects = visible
  override def unownedVects = invisible
}

case class WarpEvt(world: World, obj: Warpable) extends BoundedEvent {
  def bounds = obj.bounds
}

case class WarpStateChangeEvt(world: World, newObj: Warpable) extends BoundedEvent {
  def bounds = newObj.bounds
}

case class ObjVisibleEvt(team: Team, world: World, obj: WObject) extends VisibleEvent {
  override def visibleBy(owner: Owner) = owner.team === team
}

case class MoveEvt(
  world: World, oldObj: MovableWObject, to: Vect2, movesLeft: TileDistance
) extends Event {
  override def asViewedBy(owner: Owner) =
    if (world.isVisibleFor(owner, oldObj.position)) Iterable(this)
    else if (world.isVisibleFor(owner, to))
      Iterable(ObjVisibleEvt(owner.team, world, oldObj), this)
    else Iterable.empty
}

case class AttackEvt[D <: OwnedObj](
  world: World, attacker: Fighter, defender: (D, Option[D]), attack: Attack
) extends Event {
  override def asViewedBy(owner: Owner) =
    if (world.isVisiblePartial(owner, defender._1.bounds)) {
      if (world.isVisiblePartial(owner, attacker.bounds))
        // Just attack
        Iterable(this)
      else
        // Show, attack, then hide.
        Iterable(
          ObjVisibleEvt(owner.team, world, attacker),
          this,
          VisibilityChangeEvt(owner.team, invisible = attacker.bounds.points.toVector)
        )
    }
    else Iterable.empty
}

case class AttackedChangeEvt(
  world: World, newObj: Fighter
) extends BoundedEvent {
  def bounds = newObj.bounds
}

case class MovementChangeEvt(
  world: World, changedObj: MovableWObject
) extends BoundedEvent {
  def bounds = changedObj.bounds
}

case class MovedOrAttackedChangeEvt(
  world: World, changedObj: MoveAttackActioned
) extends BoundedEvent {
  def bounds = changedObj.bounds
}

case class ResourceChangeEvt(
  obj: Either[(World, WObject), Human], newValue: Resources
) extends VisibleEvent {
  override def visibleBy(owner: Owner) = obj.fold(
    { case (world, wObj) => world.isVisiblePartial(owner, wObj.bounds) },
    owner.isFriendOf
  )
}

case class ActionsChangeEvt(
  human: Human, actions: Actions
) extends VisibleEvent {
  override def visibleBy(owner: Owner) = human.isFriendOf(owner)
}

case class TurnEndedChangeEvt(
  human: Human, turnEnded: Boolean
) extends VisibleEvent {
  override def visibleBy(owner: Owner) = human.isFriendOf(owner)
}
