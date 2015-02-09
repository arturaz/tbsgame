package app.models.game

import akka.event.LoggingAdapter
import app.algorithms.Pathfinding
import app.algorithms.Pathfinding.Path
import app.models.game.Game.States
import app.models.game.events._
import app.models.game.world.WObject.Id
import app.models.game.world._
import implicits._
import monocle.Lenser
import monocle.syntax._
import utils.data.NonEmptyVector

import scala.reflect.ClassTag
import scalaz.\/

object Game {
  private[this] def lenser = Lenser[Game]
  val world = lenser(_.world)
  val states = lenser(_.states)

  type ResultT[A] = Either[String, Evented[A]]
  type Result = ResultT[Game]
  private type States = Map[Human, GameHumanState]
  type ObjectivesMap = Map[Team, Objectives]

  def apply(
    world: World, startingHuman: Human, startingResources: Resources,
    objectives: ObjectivesMap
  ): Either[String, Game] = {
    val humans = world.humans + startingHuman
    humans.foldLeft(Evented(world).right[String]) {
      case (left: Left[_, _], _) => left
      case (Right(eWorld), human) =>
        eWorld.map(_.addResources(human, startingResources)).extractFlatten
    }.right.map { evtWorld =>
      apply(evtWorld.value, startingStates(world, humans), objectives)
    }
  }

  def startingState(world: World, human: Human) =
    GameHumanState(world.actionsFor(human), turnEnded = false)

  def startingStates(
    world: World, humans: Iterable[Human]
  ): States = humans.map { human =>
    human -> startingState(world, human)
  }.toMap

  def canMove(human: Human, obj: Movable) = obj.owner === human
  def canAttack(human: Human, obj: Fighter) = human.isFriendOf(obj.owner)
  def canSpecial(human: Human, obj: SpecialAction) = obj.owner === human

  private object Withs {
    def withActions(human: Human, actionsNeeded: Actions, state: GameHumanState)(
      f: GameHumanState => Game.Result
    ): Game.Result = {
      if (state.actions < actionsNeeded)
        s"Not enough actions: needed $actionsNeeded, had $state".left
      else {
        val newState =
          state |-> GameHumanState.actions modify (_ - actionsNeeded)
        val events =
          if (actionsNeeded > Actions(0))
            Vector(ActionsChangeEvt(human, newState.actions))
          else
            Vector.empty
        f(newState).right.map(events ++: _)
      }
    }

    def withSpecialAction[A <: SpecialAction](
      human: Human, state: GameHumanState
    )(f: A => GameHumanState => Game.Result)(obj: A): Game.Result =
      withActions(human, obj.stats.specialActionsNeeded, state)(f(obj))

    def withResources(
      human: Human, resourcesNeeded: Resources, world: World
    )(f: Evented[World] => Game.Result): Game.Result =
      world.subResources(human, resourcesNeeded).right.flatMap(f)

    def withPopulation(
      human: Human, populationNeeded: Population, world: World
    )(f: Evented[World] => Game.Result): Game.Result = {
      val population = world.populationFor(human)
      if (population.left >= populationNeeded) f(Evented(
        world,
        PopulationChangeEvt(human, population.withValue(_ + populationNeeded))
      ))
      else
        s"Needed $populationNeeded, but $human only had $population!".left
    }

    def withWarpable(
      human: Human, warpable: WarpableStats, world: World
    )(f: Evented[World] => Game.Result): Game.Result = {
      withResources(human, warpable.cost, world) { evtWorld =>
      withPopulation(human, warpable.populationCost, evtWorld.value) { evtWorld2 =>
        f(evtWorld.events ++: evtWorld2)
      } }
    }
  }
}

trait GameLike[+A] {
  def isJoined(human: Human): Boolean
  def join(human: Human, startingResources: Resources): Game.ResultT[A]
  def leave(human: Human): Game.ResultT[A]
  def warp(
    human: Human, position: Vect2, warpable: WarpableCompanion.Some
  ): Game.ResultT[A]
  def move(human: Human, id: WObject.Id, path: NonEmptyVector[Vect2]): Game.ResultT[A]
  def attack(human: Human, id: WObject.Id, targetId: WObject.Id): Game.ResultT[A]
  def moveAttack(
    human: Human, id: WObject.Id, path: NonEmptyVector[Vect2], targetId: WObject.Id
  ): Game.ResultT[A]
  def special(human: Human, id: WObject.Id): Game.ResultT[A]
  def endTurn(human: Human): Game.ResultT[A]
}

case class Game private (
  world: World, states: Game.States, objectives: Game.ObjectivesMap
) extends GameLike[Game] {
  import app.models.game.Game.Withs._

  private[this] def fromWorld(w: Evented[World]) = w.map(updated)

  private[this] def recalculateHumanStatesOnTeamTurnStart
  (team: Team)(g: Evented[Game]): Evented[Game] =
    g.flatMap { game =>
      val teamStates = game.states.filterKeys(_.team === team)
      teamStates.foldLeft(Evented(game.states)) { case (e, (human, state)) =>
        val newState = state.copy(
          actions = game.world.actionsFor(human), turnEnded = false
        )
        if (state === newState) e
        else e.flatMap { curStates => Evented(
          curStates + (human -> newState),
          Vector(
            TurnEndedChangeEvt(human, newState.turnEnded),
            ActionsChangeEvt(human, newState.actions)
          )
        ) }
      }.map(game.updated)
    }

  def gameTurnStarted(implicit log: LoggingAdapter) = world.gameTurnStarted |> fromWorld
  def gameTurnFinished(implicit log: LoggingAdapter) = world.gameTurnFinished |> fromWorld
  def teamTurnStarted(team: Team)(implicit log: LoggingAdapter) =
    world.teamTurnStarted(team) |> fromWorld |>
    recalculateHumanStatesOnTeamTurnStart(team)
  def teamTurnFinished(team: Team)(implicit log: LoggingAdapter)
  : Evented[Winner \/ Game] =
    (world.teamTurnFinished(team) |> fromWorld).flatMap { game =>
      val remaining = game.remainingObjectives(team)
      if (remaining.someCompleted) Evented(Winner(team).leftZ, GameWonEvt(team))
      else Evented(game.rightZ)
    }

  def winner: Option[Team] = {
    world.objects.collect {
      case obj: OwnedObj if obj.stats.isCritical => obj.owner.team
    } match {
      case s if s.size === 1 => Some(s.head)
      case _ => None
    }
  }

  private[this] def teamStates(team: Team) = states.view.filter(_._1.team === team)

  def actionsLeftFor(team: Team) = teamStates(team).map(_._2.actions).sum
  /* Checks if all players have sent the turn ended flag. */
  def allPlayersTurnEnded(team: Team) = teamStates(team).forall(_._2.turnEnded)

  override def isJoined(human: Human) = states.contains(human)

  override def join(human: Human, startingResources: Resources) = {
    def evt(newState: GameHumanState) = Evented(
      updated(world, human -> newState),
      JoinEvt(
        human,
        Some(HumanState(startingResources, world.populationFor(human), newState))
      )
    )

    states.get(human).fold2(
      evt(Game.startingState(world, human)).map { g =>
        if (g.world.resourcesMap.contains(human)) Evented(g).right
        else g.world.addResources(human, startingResources).right.map(_.map(g.updated))
      }.extractFlatten,
      state => s"$human is already in the game!".left
    )
  }

  override def leave(human: Human) = withState(human) { state =>
    Evented(updated(states - human), Vector(LeaveEvt(human))).right
  }

  override def warp(
    human: Human, position: Vect2, warpable: WarpableCompanion.Some
  ): Game.Result =
    withState(human) { state =>
    withActions(human, Warpable.ActionsNeeded, state) { state =>
    withWarpable(human, warpable, world) { evtWorld =>
    withWarpZone(human, position) {
      evtWorld.map { warpable.warpW(_, human, position).right.map { _.map {
        updated(_, human -> state)
      } } }.extract.right.map(_.flatten)
    } } } }

  override def move(
    human: Human, id: WObject.Id, to: NonEmptyVector[Vect2]
  ): Game.Result =
    withMoveObj(human, id) { obj =>
    withVisibility(human, to.v.last) {
      obj.moveTo(world, to).right.map { _.map { case (w, _) =>
        updated(w)
      } }
    } }

  override def attack(
    human: Human, id: WObject.Id, targetId: WObject.Id
  ): Game.Result =
    withAttackObj(human, id) { obj =>
    withTargetObj(human, targetId) { targetObj =>
    withVisibility(human, targetObj) {
      obj.attackW(targetObj, world).right.map { _.map { world =>
        updated(world)
      } }
    } } }

  override def moveAttack(
    human: Human, id: Id, path: NonEmptyVector[Vect2], targetId: Id
  ) = {
    for {
      evtMovedGame <- move(human, id, path).right
      evtAttackedGame <- evtMovedGame.value.attack(human, id, targetId).right
    } yield evtMovedGame.events ++: evtAttackedGame
  }

  override def special(
    human: Human, id: WObject.Id
  ): Game.Result =
    withState(human) { state =>
    withSpecialObj(human, id) {
    withSpecialAction(human, state) { obj => state =>
      obj.special(world).right.map(_.map(world => updated(world, human -> state)))
    } } }

  override def endTurn(human: Human): Game.Result =
    withState(human) { state =>
      val turnEnded = true
      Evented(
        updated(world, human -> state.copy(turnEnded = turnEnded)),
        TurnEndedChangeEvt(human, turnEnded)
      ).right
    }

  def movementFor(obj: Movable): Vector[Path] =
    Pathfinding.movement(
      obj, world.bounds, world.objects
    ).filter(_.vects.forall(world.visibilityMap.isVisible(obj.owner, _)))

  def visibleBy(owner: Owner) =
    copy(world = world.visibleBy(owner), states = states.filterKeys(_.isFriendOf(owner)))

  def remainingObjectives(team: Team) =
    objectives.getOrElse(team, Objectives.empty).remaining(this, team)

  def updated(world: World): Game = copy(world = world)
  def updated(states: States): Game = copy(states = states)
  def updated(world: World, human: (Human, GameHumanState)): Game =
    copy(world = world, states = states + human)

  private[this] def withState(human: Human)(f: GameHumanState => Game.Result) =
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

  private[this] def withWarpZone(
    human: Human, position: Vect2
  )(f: => Game.Result): Game.Result =
    if (world.isValidForWarp(human, position)) f
    else s"$human cannot warp into $position".left

  private[this] type ObjFn[A] = A => Game.Result

  private[this] def withObj[A <: OwnedObj : ClassTag](
    id: WObject.Id
  )(f: ObjFn[A]): Game.Result = {
    world.find {
      case obj: A if obj.id === id => obj
    }.fold2(s"Cannot find object with id $id".left, f)
  }

  private[this] def withCheckedObj[A <: OwnedObj : ClassTag](
    id: WObject.Id, f: ObjFn[A]
  )(checker: A => Option[String]): Game.Result = {
    withObj[A](id) { obj =>
      checker(obj).fold2(
        f(obj),
        err => s"Obj $id does not pass the checker: $err".left
      )
    }
  }

  private[this] def withMoveObj(human: Human, id: WObject.Id)(
    f: ObjFn[OwnedObj with Movable]
  ): Game.Result = withCheckedObj(id, f) { obj =>
    (!Game.canMove(human, obj)).opt(s"$human can't move $obj")
  }

  private[this] def withAttackObj(human: Human, id: WObject.Id)(
    f: ObjFn[OwnedObj with Fighter]
  ): Game.Result = withCheckedObj(id, f) { obj =>
    (!Game.canAttack(human, obj)).opt(s"$human can't attack with $obj")
  }

  private[this] def withSpecialObj(human: Human, id: WObject.Id)(
    f: ObjFn[OwnedObj with SpecialAction]
  ): Game.Result = withCheckedObj(id, f) { obj =>
    (!Game.canSpecial(human, obj)).opt(s"$human can't do special with $obj")
  }

  private[this] def withTargetObj(human: Owner, id: WObject.Id)(
    f: ObjFn[OwnedObj]
  ): Game.Result = withObj[OwnedObj](id) { obj =>
    if (human.isEnemyOf(obj.owner)) f(obj)
    else s"Cannot target friendly $obj for attack!".left
  }
}
