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

  def apply(world: World, startingResources: Int): Either[String, Game] = {
    world.humans.foldLeft(Evented(world).right[String]) {
      case (left: Left[_, _], _) => left
      case (Right(eWorld), human) =>
        eWorld.map(_.addResources(human, startingResources)).extractFlatten
    }.right.map { evtWorld =>
      apply(evtWorld.value, startingStates(world, world.humans))
    }
  }

  def startingState(world: World, human: Human) =
    HumanState(world.actionsFor(human))

  def startingStates(
    world: World, humans: Iterable[Human]
  ): States = humans.map { human =>
    human -> startingState(world, human)
  }.toMap

  private object Withs {
    def withActions(human: Human, actionsNeeded: Int, state: HumanState)(
      f: HumanState => Game.Result
    ): Game.Result = {
      if (state.actions < actionsNeeded)
        s"Not enough actions: needed $actionsNeeded, had $state".left
      else {
        val newState =
          state |-> HumanState.actions modify (_ - actionsNeeded)
        val events =
          if (actionsNeeded > 0) Vector(ActionsChangeEvt(human, newState.actions))
          else Vector.empty
        f(newState).right.map(events ++: _)
      }
    }

    def withMoveAttackAction[A <: MoveAttackActioned](
      human: Human, state: HumanState
    )(f: (A, HumanState) => Game.Result)(obj: A): Game.Result =
      withActions(
        human,
        if (obj.movedOrAttacked) 0 else obj.companion.moveAttackActionsNeeded,
        state
      )(f(obj, _))

    def withSpecialAction[A <: SpecialAction](
      human: Human, state: HumanState
    )(f: A => HumanState => Game.Result)(obj: A): Game.Result =
      withActions(human, obj.companion.specialActionsNeeded, state)(f(obj))

    def withResources(
      human: Human, resourcesNeeded: Int, world: World
    )(f: Evented[World] => Game.Result): Game.Result =
      world.subResources(human, resourcesNeeded).right.flatMap(f)
  }
}

trait GameLike[A] {
  def join(human: Human, startingResources: Int): Game.ResultT[A]
  def leave(human: Human): Game.ResultT[A]

  def warp(
    human: Human, position: Vect2, warpable: WarpableCompanion[_ <: Warpable]
  ): Game.ResultT[A]

  def move(human: Human, id: WObject.Id, to: Vect2): Game.ResultT[A]
  def attack(human: Human, id: WObject.Id, targetId: WObject.Id): Game.ResultT[A]
  def special(human: Human, id: WObject.Id): Game.ResultT[A]
  def consumeActions(human: Human): Game.ResultT[A]
}

case class Game private (
  world: World, states: Game.States
) extends GameLike[Game] with TurnBased[Game] {
  import app.models.game.Game.Withs._

  private[this] def fromWorld(w: Evented[World]) = w.map(updated)

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
            Vector(ActionsChangeEvt(human, newActions))
          )
        }
      }.map(game.updated)
    }

  def gameTurnStarted = world.gameTurnStarted |> fromWorld
  def gameTurnFinished = world.gameTurnFinished |> fromWorld
  def teamTurnStarted(team: Team) =
    world.teamTurnStarted(team) |> fromWorld |> recalculateActions(team)
  def teamTurnFinished(team: Team) = world.teamTurnFinished(team) |> fromWorld

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
    )

    states.get(human).fold2(
      evt(Game.startingState(world, human)).map { g =>
        if (g.world.resourcesMap.contains(human)) Evented(g).right
        else g.world.addResources(human, startingResources).right.map(_.map(updated))
      }.extractFlatten,
      state => s"$human is already in the game!".left
    )
  }

  def leave(human: Human) = withState(human) { state =>
    Evented(updated(states - human), Vector(LeaveEvt(human))).right
  }

  def warp(
    human: Human, position: Vect2, warpable: WarpableCompanion[_ <: Warpable]
  ): Game.Result =
    withState(human) { state =>
    withActions(human, 1, state) { state =>
    withResources(human, warpable.cost, world) { evtWorld =>
    withWarpVisibility(human, position) {
      evtWorld.map { warpable.warpW(_, human, position).right.map { _.map {
        updated(_, human -> state)
      } } }.extract.right.map(_.flatten)
    } } } }

  def move(
    human: Human, id: WObject.Id, to: Vect2
  ): Game.Result =
    withState(human) { state =>
    withMoveObj(human, id) {
    withMoveAttackAction(human, state) { (obj, state) =>
    withVisibility(human, to) {
      obj.moveTo(world, to).right.map { _.map { case (w, _) =>
        updated(w, human -> state)
      } }
    } } } }

  def attack(
    human: Human, id: WObject.Id, targetId: WObject.Id
  ): Game.Result =
    withState(human) { state =>
    withAttackObj(human, id) {
    withMoveAttackAction(human, state) { (obj, state) =>
    withTargetObj(targetId) { targetObj =>
    withVisibility(human, targetObj) {
      obj.attackW(targetObj, world).right.map { _.map { world =>
        updated(world, human -> state)
      } }
    } } } } }

  def special(
    human: Human, id: WObject.Id
  ): Game.Result =
    withState(human) { state =>
    withSpecialObj(human, id) {
    withSpecialAction(human, state) { obj => state =>
      obj.special(world).right.map(_.map(world => updated(world, human -> state)))
    } } }

  def consumeActions(human: Human): Game.Result =
    withState(human) { state =>
      val actions = 0
      Evented(
        updated(world, human -> state.copy(actions = 0)),
        Vector(ActionsChangeEvt(human, actions))
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
  )(f: => Game.Result): Game.Result =
    if (world.isVisibleFor(human, position)) f
    else s"$human does not see $position".left

  private[this] def withVisibility(
    human: Human, obj: OwnedObj
  )(f: => Game.Result): Game.Result =
    if (world.isVisiblePartial(human, obj.bounds)) f
    else s"$human does not see $obj".left

  private[this] def withWarpVisibility(
    human: Human, position: Vect2
  )(f: => Game.Result): Game.Result =
    if (world.isValidForWarp(human, position)) f
    else s"$human cannot warp into $position".left

  private[this] type ObjFn[A] = A => Game.Result

  private[this] def withObj[A <: OwnedObj : ClassTag](
    id: WObject.Id
  )(f: ObjFn[A]): Game.Result = {
    world.find {
      case obj: A if obj.id == id => obj
    }.fold2(s"Cannot find object with id $id".left, f)
  }

  private[this] def withHumanObj[A <: OwnedObj : ClassTag](
    human: Human, id: WObject.Id
  )(f: ObjFn[A]): Game.Result = {
    withObj[A](id) { obj =>
      if (obj.owner == human) f(obj)
      else s"Cannot find object belonging to $human with id $id".left
    }
  }

  private[this] def withMoveObj(human: Human, id: WObject.Id)(
    f: ObjFn[OwnedObj with MovableWObject]
  ): Game.Result = withHumanObj(human, id)(f)

  private[this] def withAttackObj(human: Human, id: WObject.Id)(
    f: ObjFn[OwnedObj with Fighter]
  ): Game.Result = withHumanObj(human, id)(f)

  private[this] def withSpecialObj(human: Human, id: WObject.Id)(
    f: ObjFn[OwnedObj with SpecialAction]
  ): Game.Result = withHumanObj(human, id)(f)

  private[this] def withTargetObj(id: WObject.Id)(
    f: ObjFn[OwnedObj with SpecialAction]
  ): Game.Result = withObj(id)(f)
}
