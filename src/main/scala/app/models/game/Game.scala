package app.models.game

import app.algorithms.Pathfinding
import app.models.game.Game.States
import app.models.{TurnBased, Team, Owner, Player}
import app.models.game.events._
import app.models.world._
import implicits._
import monocle.Lenser
import monocle.syntax._

import scala.reflect.ClassTag

object Game {
  private[this] def lenser = Lenser[Game]
  val world = lenser(_.world)
  val states = lenser(_.states)

  type ResultT[A] = Either[String, Evented[A]]
  type Result = ResultT[Game]
  private type States = Map[Player, PlayerState]

  def apply(world: World, startingResources: Int): Game = apply(
    world, startingStates(world, startingResources, world.players)
  )

  def startingStates(
    world: World, startingResources: Int, players: Iterable[Player]
  ): States = players.map { player =>
    player -> PlayerState(startingResources, world.actionsFor(player))
  }.toMap

  private object Withs {
    private[this] def updateStates(game: Game, events: Events): Game = {
      val players = game.states.keys.map(p => p.id -> p).toMap
      def update(states: Game.States, id: Owner.Id)(
        f: PlayerState => PlayerState
        ) = {
        val player = players(id)
        val state = states(player)
        states + (player -> f(state))
      }

      val newStates = events.foldLeft(game.states) {
        case (fStates, ResourceChangeEvt(Right(id), diff)) =>
          update(fStates, id)(_ |-> PlayerState.resources modify (_ + diff))
        case (fStates, _) => fStates
      }
      game.copy(states = newStates)
    }

    def updateStates(evented: Evented[Game]): Evented[Game] =
      evented.mapE(updateStates)

    def withStateChanges(f: => Game.Result): Game.Result =
      f.right.map(updateStates)

    def withActions(playerId: Owner.Id, actionsNeeded: Int)(
      f: PlayerState => Game.Result
    )(state: PlayerState): Game.Result = {
      if (state.actions < actionsNeeded)
        s"Not enough actions: needed $actionsNeeded, had $state".left
      else {
        val newState =
          state |-> PlayerState.actions modify (_ - actionsNeeded)
        val events =
          if (actionsNeeded > 0) Vector(ActionChangeEvt(playerId, newState.actions))
          else Vector.empty
        f(newState).right.map(events +: _)
      }
    }

    def withMoveAttackAction[A <: MoveAttackActioned](
      playerId: Owner.Id
    )(f: (A, PlayerState) => Game.Result)(obj: A)(state: PlayerState): Game.Result =
      withActions(
        playerId,
        if (obj.movedOrAttacked) 0 else obj.companion.moveAttackActionsNeeded
      )(f(obj, _))(state)

    def withSpecialAction[A <: SpecialAction](
      playerId: Owner.Id
    )(f: A => PlayerState => Game.Result)(obj: A)(state: PlayerState): Game.Result =
      withActions(playerId, obj.companion.specialActionsNeeded)(f(obj))(state)

    def withResources(
      resourcesNeeded: Int
    )(f: PlayerState => Game.Result)(state: PlayerState): Game.Result = {
      if (state.resources < resourcesNeeded)
        s"Not enough resources: needed $resourcesNeeded, had $state".left
      else f(
        state |-> PlayerState.resources modify (_ - resourcesNeeded)
      )
    }
  }
}

trait GameLike[A] {
  def warp(
    player: Player, position: Vect2, warpable: WarpableCompanion[_ <: Warpable]
  ): Game.ResultT[A]

  def move(
    player: Player, from: Vect2, to: Vect2
  ): Game.ResultT[A]

  def attack(
    player: Player, source: Vect2, target: Vect2
  ): Game.ResultT[A]

  def special(
    player: Player, position: Vect2
  ): Game.ResultT[A]
}

case class Game private (
  world: World, states: Game.States
) extends GameLike[Game] with TurnBased[Game] {
  import Game.Withs._

  private[this] def fromWorldEvents(w: Evented[World]) =
    w.map(updated) |> updateStates
  private[this] def recalculatePlayerStates
  (team: Team)(g: Evented[Game]): Evented[Game] =
    g.map { game =>
      val players = game.states.filterKeys(_.team == team)
      val states = players.map { case (player, state) =>
        player -> state.copy(actions = game.world.actionsFor(player))
      }
      game.updated(states)
    }

  def gameTurnStarted = world.gameTurnStarted |> fromWorldEvents
  def gameTurnFinished = world.gameTurnFinished |> fromWorldEvents
  def teamTurnStarted(team: Team) =
    world.teamTurnStarted(team) |> fromWorldEvents |> recalculatePlayerStates(team)
  def teamTurnFinished(team: Team) = world.teamTurnFinished(team) |> fromWorldEvents

  def winner: Option[Team] = {
    world.objects.collect {
      case obj: OwnedObj if obj.companion.isCritical => obj.owner.team
    } match {
      case s if s.size == 1 => Some(s.head)
      case _ => None
    }
  }

  def warp(
    player: Player, position: Vect2, warpable: WarpableCompanion[_ <: Warpable]
  ): Game.Result =
    withStateChanges {
    withState(player) {
    withActions(player.id, 1) {
    withResources(warpable.cost) { state =>
      warpable.warp(world, player, position).right.map { warpedIn =>
        Evented(
          updated(world.add(warpedIn), player -> state),
          Vector(WarpEvt(warpedIn))
        )
      }
    } } } }

  def move(
    player: Player, from: Vect2, to: Vect2
  ): Game.Result =
    withStateChanges {
    withState(player) {
    withMoveObj(player, from) {
    withMoveAttackAction(player.id) { (obj, state) =>
      Pathfinding.aStar(
        obj, to.toBounds, world.bounds, obj.obstacles(world.objects).map(_.bounds)
      ).fold2(
        s"Can't find path from $from to $to for $obj".left,
        path => obj.moveTo(to).right.map { moved =>
          Evented(
            updated(world.updated(obj, moved), player -> state),
            MoveEvt(obj, path)
          )
        }
      )
    } } } }

  def attack(
    player: Player, source: Vect2, target: Vect2
  ): Game.Result =
    withStateChanges {
    withState(player) {
    withAttackObj(player, source) {
    withMoveAttackAction(player.id) { (obj, state) =>
      world.find {
        case targetObj: OwnedObj if targetObj.bounds.contains(target) =>
          targetObj
      }.fold2(
        s"Can't find target at $target for $player".left,
        targetObj => obj.attack(targetObj).right.map {
        case (attack, attacked) =>
          Evented(
            updated(
              world.updated(obj, attacked).update(attack, targetObj),
              player -> state
            ),
            Vector(AttackEvt(obj.id, targetObj.id, attack))
          )
        }
      )
    } } } }

  def special(
    player: Player, position: Vect2
  ): Game.Result =
    withStateChanges {
    withState(player) {
    withSpecialObj(player, position) {
    withSpecialAction(player.id) { obj => state =>
      obj.special(world).right.map(_.map(world => updated(world, player -> state)))
    } } } }

  private[this] def updated(world: World): Game = copy(world = world)
  private def updated(states: States): Game = copy(states = states)
  private[this] def updated(world: World, player: (Player, PlayerState)): Game =
    copy(world = world, states = states + player)

  private[this] def withState(player: Player)(f: PlayerState => Game.Result) =
    states.get(player).fold2(Left(s"No player state for $player: $states"), f)

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
