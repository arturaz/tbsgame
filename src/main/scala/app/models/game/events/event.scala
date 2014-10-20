package app.models.game.events

import app.models.game._
import app.models.game.world._
import implicits._

sealed trait Event {
  def visibleBy(owner: Owner): Boolean
}

sealed trait AlwaysVisibleEvent extends Event {
  override def visibleBy(owner: Owner) = true
}

sealed trait BoundedEvent extends Event {
  def world: World
  def bounds: Bounds
  def visibleBy(owner: Owner) = world.isVisiblePartial(owner, bounds)
}

case class HumanState(resources: Resources, gameState: GameHumanState)

case class JoinEvt(human: Human, state: HumanState) extends AlwaysVisibleEvent
case class LeaveEvt(human: Human) extends AlwaysVisibleEvent
case class TurnStartedEvt(team: Team) extends AlwaysVisibleEvent
case class TurnEndedEvt(team: Team) extends AlwaysVisibleEvent

case class VisibilityChangeEvt(
  team: Team,
  visiblePositions: Vector[Vect2]=Vector.empty,
  invisiblePositions: Vector[Vect2]=Vector.empty
) extends Event {
  override def visibleBy(owner: Owner) = owner.team === team
}

case class WarpEvt(world: World, obj: Warpable) extends BoundedEvent {
  def bounds = obj.bounds
}

case class ObjVisibleEvt(world: World, obj: WObject) extends BoundedEvent {
  def bounds = obj.bounds
}

case class MoveEvt(
  world: World, oldObj: MovableWObject, to: Vect2, movesLeft: TileDistance
) extends Event {
  override def visibleBy(owner: Owner) =
    world.isVisibleFor(owner, oldObj.position) || world.isVisibleFor(owner, to)
}

case class AttackEvt[D <: OwnedObj](
  world: World, attacker: Fighter, defender: (D, Option[D]), attack: Attack
) extends BoundedEvent {
  def bounds = defender._1.bounds
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
) extends Event {
  override def visibleBy(owner: Owner) = obj.fold(
    { case (world, wObj) => world.isVisiblePartial(owner, wObj.bounds) },
    owner.isFriendOf
  )
}

case class ActionsChangeEvt(
  human: Human, actions: Actions
) extends Event {
  override def visibleBy(owner: Owner) = human.isFriendOf(owner)
}
