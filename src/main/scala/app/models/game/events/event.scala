package app.models.game.events

import app.models.game._
import app.models.game.world._
import app.models.game.world.maps.VisibilityMap
import implicits._
import utils.ValWithMax
import utils.data.Timeframe

import scalaz.\/

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
  def visibilityMap: VisibilityMap
  def bounds: Bounds
  override def visibleBy(owner: Owner) = visibilityMap.isVisiblePartial(owner, bounds)
}

case class HumanState(
  resources: Resources, population: ValWithMax[Population], gameState: GamePlayerState
)

case class JoinEvt(human: Human, state: Option[HumanState]) extends Event {
  override def asViewedBy(owner: Owner) =
    if (owner.isFriendOf(human)) Seq(this)
    else Seq(copy(state = None))
}
case class LeaveEvt(human: Human) extends AlwaysVisibleEvent

case class TurnStartedEvt(team: Team) extends AlwaysVisibleEvent
case class TurnEndedEvt(team: Team) extends AlwaysVisibleEvent

case class SetTurnTimerEvt(whom: Human \/ Team, timeframe: Timeframe)
extends VisibleEvent {
  override def visibleBy(owner: Owner) = whom.fold(
    // If this is a turn timer for human, send it only if it directly belongs to that
    // human.
    human => owner === human,
    // If this is a turn timer for team, it is for enemy team and only send it if owner
    // does not belong to this team.
    team => owner.team =/= team
  )
}

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

case class WarpEvt(visibilityMap: VisibilityMap, obj: Warpable) extends BoundedEvent {
  def bounds = obj.bounds
}

case class WarpStateChangeEvt(
  visibilityMap: VisibilityMap, newObj: Warpable
) extends BoundedEvent {
  def bounds = newObj.bounds
}

case class ObjVisibleEvt(team: Team, obj: WObject) extends VisibleEvent {
  override def visibleBy(owner: Owner) = owner.team === team
}

trait ObjDestroyedEvt extends VisibleEvent {
  def obj: WObject
}
/* Real object was destroyed - dispatch to everyone who sees it. */
case class RealObjDestroyedEvt(
  visibilityMap: VisibilityMap, obj: WObject
) extends ObjDestroyedEvt with BoundedEvent {
  override def bounds = obj.bounds
}
/* Ghost object was destroyed - one that we saw in the past but now we can see that it
   does not exist anymore - dispatch to one team. */
case class GhostObjDestroyedEvt(
  dispatchFor: Team, obj: WObject
) extends ObjDestroyedEvt {
  override def visibleBy(owner: Owner) = owner.team === dispatchFor
}

case class MoveEvt(
  visibilityMap: VisibilityMap, oldObj: Movable, to: Vect2, movesLeft: Movement
) extends Event {
  override def asViewedBy(owner: Owner) =
    if (visibilityMap.isVisible(owner, oldObj.position)) Iterable(this)
    else if (visibilityMap.isVisible(owner, to))
      Iterable(ObjVisibleEvt(owner.team, oldObj), this)
    else Iterable.empty
}

case class AttackEvt[D <: OwnedObj](
  visibilityMap: VisibilityMap, attacker: Fighter, defender: (D, Option[D]), attack: Attack
) extends Event {
  override def asViewedBy(owner: Owner) =
    if (visibilityMap.isVisiblePartial(owner, defender._1.bounds)) {
      if (visibilityMap.isVisiblePartial(owner, attacker.bounds))
        // Just attack
        Iterable(this)
      else
        // Show, attack, then hide.
        Iterable(
          ObjVisibleEvt(owner.team, attacker),
          this,
          VisibilityChangeEvt(owner.team, invisible = attacker.bounds.points.toVector)
        )
    }
    else Iterable.empty
}

case class AttacksChangedEvt(
  visibilityMap: VisibilityMap, newObj: Fighter
) extends BoundedEvent {
  def bounds = newObj.bounds
}

case class LevelChangeEvt(
  visibilityMap: VisibilityMap, newObj: Fighter
) extends BoundedEvent {
  def bounds = newObj.bounds
}

case class HPChangeEvt(
  visibilityMap: VisibilityMap, newObj: OwnedObj
) extends BoundedEvent {
  def bounds = newObj.bounds
}

case class OwnerChangeEvt(
  visibilityMap: VisibilityMap, newObj: OwnedObj
) extends BoundedEvent {
  def bounds = newObj.bounds
}

case class MovementChangeEvt(
  visibilityMap: VisibilityMap, changedObj: Movable
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
  player: Player, actions: Actions
) extends VisibleEvent {
  override def visibleBy(owner: Owner) = player.isFriendOf(owner)
}

case class PopulationChangeEvt(
  player: Player, population: ValWithMax[Population]
) extends VisibleEvent {
  override def visibleBy(owner: Owner) = owner.isFriendOf(player)
}

case class TurnEndedChangeEvt(
  player: Player, turnEnded: Boolean
) extends VisibleEvent {
  override def visibleBy(owner: Owner) = player.isFriendOf(owner)
}

case class ObjectivesUpdatedEvt(
  team: Team, objectives: RemainingObjectives
) extends VisibleEvent {
  override def visibleBy(owner: Owner) = owner.team === team
}

case class GameWonEvt(team: Team) extends AlwaysVisibleEvent