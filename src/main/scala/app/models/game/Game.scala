package app.models.game

import app.models._
import app.models.game.Game.States
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
  private type States = Map[Human, HumanState]

  def apply(world: World, startingResources: Int): Game = apply(
    world, startingStates(world, startingResources, world.humans)
  )

  def startingState(world: World, startingResources: Int, human: Human) =
    HumanState(startingResources, world.actionsFor(human))

  def startingStates(
    world: World, startingResources: Int, humans: Iterable[Human]
  ): States = humans.map { human =>
    human -> startingState(world, startingResources, human)
  }.toMap

  private object Withs {
    private[this] def updateHumanStates(game: Game, events: Events): Game = {
      val humans = game.states.keys.map(h => h.id -> h).toMap
      def update(states: Game.States, human: Human)(
        f: HumanState => HumanState
      ) = states + (human -> f(states(human)))

      val newStates = events.foldLeft(game.states) {
        case (fStates, ResourceChangeEvt(Right(human), diff)) =>
          update(fStates, human)(_ |-> HumanState.resources modify (_ + diff))
        case (fStates, _) => fStates
      }
      game.copy(states = newStates)
    }

    def updateHumanStates(evented: Evented[Game]): Evented[Game] =
      evented.mapE(updateHumanStates)

    def withStateChanges(f: => Game.Result): Game.Result =
      f.right.map(updateHumanStates)

    def withActions(human: Human, actionsNeeded: Int)(
      f: HumanState => Game.Result
    )(state: HumanState): Game.Result = {
      if (state.actions < actionsNeeded)
        s"Not enough actions: needed $actionsNeeded, had $state".left
      else {
        val newState =
          state |-> HumanState.actions modify (_ - actionsNeeded)
        val events =
          if (actionsNeeded > 0) Vector(ActionChangeEvt(human, newState.actions))
          else Vector.empty
        f(newState).right.map(events ++: _)
      }
    }

    def withMoveAttackAction[A <: MoveAttackActioned](
      human: Human
    )(f: (A, HumanState) => Game.Result)(obj: A)(state: HumanState): Game.Result =
      withActions(
        human,
        if (obj.movedOrAttacked) 0 else obj.companion.moveAttackActionsNeeded
      )(f(obj, _))(state)

    def withSpecialAction[A <: SpecialAction](
      human: Human
    )(f: A => HumanState => Game.Result)(obj: A)(state: HumanState): Game.Result =
      withActions(human, obj.companion.specialActionsNeeded)(f(obj))(state)

    def withResources(
      resourcesNeeded: Int
    )(f: HumanState => Game.Result)(state: HumanState): Game.Result = {
      if (state.resources < resourcesNeeded)
        s"Not enough resources: needed $resourcesNeeded, had $state".left
      else f(
        state |-> HumanState.resources modify (_ - resourcesNeeded)
      )
    }
  }
}

trait GameLike[A] {
  def join(human: Human, startingResources: Int): Game.ResultT[A]
  def leave(human: Human): Game.ResultT[A]

  def warp(
    human: Human, position: Vect2, warpable: WarpableCompanion[_ <: Warpable]
  ): Game.ResultT[A]

  def move(human: Human, from: Vect2, to: Vect2): Game.ResultT[A]
  def attack(human: Human, source: Vect2, target: Vect2): Game.ResultT[A]
  def special(human: Human, position: Vect2): Game.ResultT[A]
  def consumeActions(human: Human): Game.ResultT[A]
}

case class Game private (
  world: World, states: Game.States
) extends GameLike[Game] with TurnBased[Game] {
  import app.models.game.Game.Withs._

  private[this] def fromWorldEvents(w: Evented[World]) =
    w.map(updated) |> updateHumanStates

  private[this] def recalculateActions
  (team: Team)(g: Evented[Game]): Evented[Game] =
    g.flatMap { game =>
      val teamStates = game.states.filterKeys(_.team == team)
      teamStates.foldLeft(Evented(teamStates)) { case (e, (human, state)) =>
        val newActions = game.world.actionsFor(human)
        if (state.actions == newActions) e
        else e.flatMap { curStates =>
          Evented(
            curStates + (human -> state.copy(actions = game.world.actionsFor(human))),
            Vector(ActionChangeEvt(human, newActions))
          )
        }
      }.map(game.updated)
    }

  def gameTurnStarted = world.gameTurnStarted |> fromWorldEvents
  def gameTurnFinished = world.gameTurnFinished |> fromWorldEvents
  def teamTurnStarted(team: Team) =
    world.teamTurnStarted(team) |> fromWorldEvents |> recalculateActions(team)
  def teamTurnFinished(team: Team) = world.teamTurnFinished(team) |> fromWorldEvents

  def winner: Option[Team] = {
    world.objects.collect {
      case obj: OwnedObj if obj.companion.isCritical => obj.owner.team
    } match {
      case s if s.size == 1 => Some(s.head)
      case _ => None
    }
  }

  def actionsLeftFor(team: Team) =
    states.view.filter(_._1.team == team).map(_._2.actions).sum

  def join(human: Human, startingResources: Int) = {
    def evt(newState: HumanState) = Evented(
      updated(world, human -> newState), Vector(JoinEvt(human, newState))
    ).right

    states.get(human).fold2(
      evt(Game.startingState(world, startingResources, human)),
      state =>
        if (state.active) s"$human is already in the game!".left
        else evt(state |-> HumanState.active set true)
    )
  }

  def leave(human: Human) = withState(human) { state =>
    Evented(
      updated(
        world, human -> (state |-> HumanState.active set false)
      ), Vector(LeaveEvt(human))
    ).right
  }

  def warp(
    human: Human, position: Vect2, warpable: WarpableCompanion[_ <: Warpable]
  ): Game.Result =
    withStateChanges {
    withState(human) {
    withActions(human, 1) {
    withResources(warpable.cost) {
    withVisibility(human, position) { state =>
      warpable.warpW(world, human, position).right.map { _.map {
        updated(_, human -> state)
      } }
    } } } } }

  def move(
    human: Human, from: Vect2, to: Vect2
  ): Game.Result =
    withStateChanges {
    withState(human) {
    withVisibility(human, to) {
    withMoveObj(human, from) {
    withMoveAttackAction(human) { (obj, state) =>
      obj.moveTo(world, to).right.map { _.map { case (w, _) =>
        updated(w, human -> state)
      } }
    } } } } }

  def attack(
    human: Human, source: Vect2, target: Vect2
  ): Game.Result =
    withStateChanges {
    withState(human) {
    withVisibility(human, target) {
    withAttackObj(human, source) {
    withMoveAttackAction(human) { (obj, state) =>
      world.find {
        case targetObj: OwnedObj if targetObj.bounds.contains(target) =>
          targetObj
      }.fold2(
        s"Can't find target at $target for $human".left,
        targetObj => obj.attackW(targetObj, world).right.map { _.map { world =>
          updated(world, human -> state)
        } }
      )
    } } } } }

  def special(
    human: Human, position: Vect2
  ): Game.Result =
    withStateChanges {
    withState(human) {
    withSpecialObj(human, position) {
    withSpecialAction(human) { obj => state =>
      obj.special(world).right.map(_.map(world => updated(world, human -> state)))
    } } } }

  def consumeActions(human: Human): Game.Result =
    withState(human) { state =>
      val actions = 0
      Evented(
        updated(world, human -> state.copy(actions = 0)),
        Vector(ActionChangeEvt(human, actions))
      ).right
    }

  private def updated(world: World): Game = copy(world = world)
  private def updated(states: States): Game = copy(states = states)
  private[this] def updated(world: World, human: (Human, HumanState)): Game =
    copy(world = world, states = states + human)

  private[this] def withState(human: Human)(f: HumanState => Game.Result) =
    states.get(human).fold2(Left(s"No state for $human: $states"), f)

  private[this] def withVisibility(
    human: Human, position: Vect2
  )(f: HumanState => Game.Result)(state: HumanState): Game.Result =
    if (world.isVisibleFor(human, position)) f(state)
    else s"$human does not see $position".left

  private[this] type ObjFn[A] = A => HumanState => Game.Result

  private[this] def withObj[A <: OwnedObj : ClassTag](
    human: Human, position: Vect2
  )(f: ObjFn[A])(state: HumanState): Game.Result = {
    world.find {
      case obj: A if obj.position == position && obj.owner == human => obj
    }.fold2(
      s"Cannot find object belonging to $human in $position".left,
      obj => f(obj)(state)
    )
  }

  private[this] def withMoveObj(human: Human, position: Vect2)(
    f: ObjFn[OwnedObj with MovableWObject]
  )(state: HumanState): Game.Result =
    withObj(human, position)(f)(state)

  private[this] def withAttackObj(human: Human, position: Vect2)(
    f: ObjFn[OwnedObj with Fighter]
  )(state: HumanState): Game.Result =
    withObj(human, position)(f)(state)

  private[this] def withSpecialObj(human: Human, position: Vect2)(
    f: ObjFn[OwnedObj with SpecialAction]
  )(state: HumanState): Game.Result =
    withObj(human, position)(f)(state)
}
