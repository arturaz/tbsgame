package app.models.game

import app.algorithms.Pathfinding
import app.models.Player
import app.models.game.events.{AttackEvt, Event, MoveEvt, WarpEvt}
import app.models.world._
import implicits._
import monocle.Lenser
import monocle.syntax._

import scala.reflect.ClassTag

object Game {
  private[this] def lenser = Lenser[Game]
  val world = lenser(_.world)
  val states = lenser(_.states)

  type Result = Either[String, (Game, Vector[Event])]
}

case class Game(world: World, states: Map[Player, PlayerState]) {
  def warp(
    player: Player, position: Vect2, warpable: WarpableStats[_ <: Warpable]
  ): Game.Result =
    withState(player) {
    withActions(1) {
    withResources(warpable.cost) { state =>
      warpable.warp(world, player, position).right.map { warpedIn =>
        (
          updated(world.add(warpedIn), player -> state),
          Vector(WarpEvt(warpedIn))
        )
      }
    } } }

  def move(
    player: Player, from: Vect2, to: Vect2
  ): Game.Result =
    withState(player) {
    withMoveObj(player, from) {
    withMoveAttackAction { (obj, state) =>
      Pathfinding.aStar(
        obj, to.toBounds, world.bounds, obj.obstacles(world.objects).map(_.bounds)
      ).fold2(
        s"Can't find path from $from to $to for $obj".left,
        path => obj.moveTo(to).right.map { moved =>
          (
            updated(world.update(obj, moved), player -> state),
            MoveEvt(obj, path)
          )
        }
      )
    } } }

  def attack(
    player: Player, source: Vect2, target: Vect2
  ): Game.Result =
    withState(player) {
    withAttackObj(player, source) {
    withMoveAttackAction { (obj, state) =>
      world.find {
        case targetObj: OwnedObj if targetObj.bounds.contains(target) =>
          targetObj
      }.fold2(
        s"Can't find target at $target for $player".left,
        targetObj => obj.attack(targetObj).right.map {
        case (attack, attacked) =>
          (
            updated(
              world.update(obj, attacked).update(attack, targetObj),
              player -> state
            ),
            Vector(AttackEvt(obj.id, targetObj.id, attack))
          )
        }
      )
    } } }

  def special(
    player: Player, position: Vect2
  ): Game.Result =
    withState(player) {
    withSpecialObj(player, position) {
    withSpecialAction { (obj, state) =>
      obj.special(world).right.map { case (newWorld, events) => (
        updated(newWorld, player -> state),
        events
      ) }
    } } }

  private[this] def updated(world: World, player: (Player, PlayerState)): Game =
    copy(world = world, states = states + player)

  private[this] def withState(player: Player)(f: PlayerState => Game.Result) =
    states.get(player).fold2(Left(s"No player state for $player: $states"), f)

  private[this] def withActions(
    actionsNeeded: Int
  )(f: PlayerState => Game.Result)(state: PlayerState): Game.Result = {
    if (state.actions.current < 1)
      s"Not enough actions: needed $actionsNeeded, had $state".left
    else f(
      state |-> PlayerState.actions |->
        PlayerState.Actions.current modify (_ - 1)
    )
  }

  private[this] def withMoveAttackAction[A <: MoveAttackActioned](
    f: (A, PlayerState) => Game.Result
  )(obj: A)(state: PlayerState): Game.Result =
    withActions(
      if (obj.movedOrAttacked) 0
      else obj.stats.moveAttackActionsNeeded
    )(f(obj, _))(state)

  private[this] def withSpecialAction[A <: SpecialAction](
    f: (A, PlayerState) => Game.Result
  )(obj: A)(state: PlayerState): Game.Result =
    withActions(obj.stats.specialActionsNeeded)(f(obj, _))(state)

  private[this] def withResources(
    resourcesNeeded: Int
  )(f: PlayerState => Game.Result)(state: PlayerState): Game.Result = {
    if (state.resources < resourcesNeeded)
      s"Not enough resources: needed $resourcesNeeded, had $state".left
    else f(
      state |-> PlayerState.resources modify (_ - resourcesNeeded)
    )
  }

  private[this] type ObjFn[A] = A => PlayerState => Game.Result

  private[this] def withObj[A <: OwnedObj : ClassTag](
    player: Player, position: Vect2
  )(f: ObjFn[A])(state: PlayerState): Game.Result = {
    world.find {
      case obj: A if obj.position == position && obj.owner == player => obj
    }.fold2(
      s"Cannot find object belonging to $player in $position".left,
      obj => f(obj)(state)
    )
  }

  private[this] def withMoveObj(player: Player, position: Vect2)(
    f: ObjFn[OwnedObj with MovableWObject]
  )(state: PlayerState): Game.Result =
    withObj(player, position)(f)(state)

  private[this] def withAttackObj(player: Player, position: Vect2)(
    f: ObjFn[OwnedObj with Fighter]
  )(state: PlayerState): Game.Result =
    withObj(player, position)(f)(state)

  private[this] def withSpecialObj(player: Player, position: Vect2)(
    f: ObjFn[OwnedObj with SpecialAction]
  )(state: PlayerState): Game.Result =
    withObj(player, position)(f)(state)
}
