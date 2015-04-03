package app.models.game

import akka.event.LoggingAdapter
import app.algorithms.Pathfinding
import app.algorithms.Pathfinding.Path
import app.models.game.Game.States
import app.models.game.GamePlayerState.CanWarp
import app.models.game.events._
import app.models.game.world.WObject.Id
import app.models.game.world._
import app.models.game.world.buildings.{ExtractorStats, LaserTowerStats, WarpLinkerStats}
import app.models.game.world.units.{ScoutStats, GunshipStats, RocketFrigateStats, CorvetteStats}
import implicits._
import monocle.Lenser
import monocle.syntax._
import utils.data.NonEmptyVector
import app.models.game.world.Ops._

import scala.reflect.ClassTag
import scalaz.\/

object Game {
  private[this] def lenser = Lenser[Game]
  val worldL = lenser(_.world)
  val statesL = lenser(_.states)

  type ResultT[A] = Either[String, Evented[A]]
  type Result = ResultT[Game]
  type States = Map[Player, GamePlayerState]
  type ObjectivesMap = Map[Team, Objectives]

  object StartingPlayer {
    val DefaultWarpSet: GamePlayerState.CanWarp = Set(
      ExtractorStats,
      WarpLinkerStats,
      LaserTowerStats,
      CorvetteStats,
      RocketFrigateStats,
      GunshipStats,
      ScoutStats
    )
  }
  case class StartingPlayer(
    player: Player, resources: Resources,
    canWarp: GamePlayerState.CanWarp=StartingPlayer.DefaultWarpSet
  )

  def apply(
    world: World, starting: Set[StartingPlayer], objectives: ObjectivesMap
  ): Either[String, Game] = {
    val startingPlayersWarpMap: Map[Player, CanWarp] =
      starting.map(sp => sp.player -> sp.canWarp)(collection.breakOut)

    starting.foldLeft(Evented(world).right[String]) {
      case (left: Left[_, _], _) => left
      case (Right(eWorld), data) =>
        eWorld.map(_.addResources(data.player, data.resources)).extractFlatten
    }.right.map { evtWorld =>
      val world = evtWorld.value
      apply(
        world, startingStates(
          world,
          world.players.map { player => (
            player,
            startingPlayersWarpMap.getOrElse(player, Set.empty[WarpableCompanion.Some])
          ) }(collection.breakOut)
        ), objectives
      )
    }
  }

  def startingState(world: World, player: Player, canWarp: GamePlayerState.CanWarp) =
    GamePlayerState(
      world.actionsFor(player),
      if (player.isHuman) GamePlayerState.WaitingForTurnEnd else GamePlayerState.TurnEnded,
      canWarp
    )

  def startingStates(
    world: World, players: Map[Player, GamePlayerState.CanWarp]
  ): States = players.map { case (player, canWarp) =>
    player -> startingState(world, player, canWarp)
  }

  def canMove(human: Human, obj: Movable) = obj.owner === human
  def canAttack(human: Human, obj: Fighter) = human.isFriendOf(obj.owner)

  private object Withs {
    def withActions(human: Human, actionsNeeded: Actions, state: GamePlayerState)(
      f: GamePlayerState => Game.Result
    ): Game.Result = {
      if (state.actions < actionsNeeded)
        s"Not enough actions: needed $actionsNeeded, had $state".left
      else {
        val newState =
          state |-> GamePlayerState.actions modify (_ - actionsNeeded)
        val events =
          if (actionsNeeded > Actions(0))
            Vector(ActionsChangeEvt(human, newState.actions))
          else
            Vector.empty
        f(newState).right.map(events ++: _)
      }
    }

    def withSpecialAction[A <: SpecialAction](
      human: Human, state: GamePlayerState
    )(f: A => GamePlayerState => Game.Result)(obj: A): Game.Result =
      withActions(human, obj.stats.specialActionsNeeded, state)(f(obj))

    def withResources(
      human: Human, resourcesNeeded: Resources, world: World
    )(f: Evented[World] => Game.Result): Game.Result =
      world.subResources(human, resourcesNeeded).right.flatMap(f)

    def withPopulation(
      human: Human, populationNeeded: Population, world: World
    )(f: Evented[World] => Game.Result): Game.Result = {
      val population = world.populationFor(human)
      if (populationNeeded.isZero || population.left >= populationNeeded) f(Evented(
        world,
        PopulationChangeEvt(human, population.withValue(_ + populationNeeded))
      ))
      else
        s"Needed $populationNeeded, but $human only had $population!".left
    }

    def withAbilityToWarp(
      human: Human, canWarp: GamePlayerState.CanWarp, warpable: WarpableCompanion.Some
    )(f: => Game.Result): Game.Result = {
      if (canWarp.contains(warpable)) f
      else s"$human cannot warp $warpable! (allowed=$canWarp)".left
    }

    def withWarpable(
      human: Human, canWarp: GamePlayerState.CanWarp, warpable: WarpableCompanion.Some,
      world: World
    )(f: Evented[World] => Game.Result): Game.Result = {
      withResources(human, warpable.cost, world) { evtWorld =>
      withPopulation(human, warpable.populationCost, evtWorld.value) { evtWorld2 =>
      withAbilityToWarp(human, canWarp, warpable) {
        f(evtWorld.events ++: evtWorld2)
      } } }
    }
  }

  private def recalculatePlayerStatesOnTeamTurnStart
  (team: Team)(g: Evented[Game]): Evented[Game] =
    g.flatMap { game =>
      val teamStates = game.states.filterKeys(_.team === team)
      teamStates.foldLeft(Evented(game.states)) { case (e, (player, state)) =>
        val newState = state.onTurnStart(player, game.world.actionsFor(player))
        if (state === newState) e
        else e.flatMap { curStates => Evented(
          curStates + (player -> newState),
          Vector(
            TurnEndedChangeEvt(player, ! newState.activity.canAct),
            ActionsChangeEvt(player, newState.actions)
          )
        ) }
      }.map(game.updated)
    }

  private def doAutoSpecials(team: Team)(g: Evented[Game])(implicit log: LoggingAdapter)
  : Evented[Game] = {
    def forPlayer(
      teamObjects: Vector[SpecialAtEndOfTurn], player: Player, state: GamePlayerState,
      startingWorld: World
    ): Evented[(World, GamePlayerState)] = {
      val objects =
        teamObjects.filter(_.canDoSpecial(player)).sortBy(_.endOfTurnPriority)
      var evented = Evented(startingWorld)
      var actions = state.actions
      def retEvt = evented.map(world => (world, state.copy(actions = actions)))

      objects.foreach { obj =>
        while (actions >= obj.stats.specialActionsNeeded) {
          obj.special(evented.value, player).fold(
            err => {
              log.error(
                "Failed to do auto special for {} on {} with state {}",
                player, obj, state
              )
              return retEvt
            },
            newEvtWorld => evented = evented.events ++: newEvtWorld
          )

          actions -= obj.stats.specialActionsNeeded
        }
      }

      retEvt
    }

    g.flatMap { game =>
      val teamStates =
        game.states.filterKeys(_.team === team).filter(_._2.actions.isPositive)
      val teamObjects = game.world.objects.collect {
        case obj: SpecialAtEndOfTurn if obj.owner.isFriendOf(team) => obj
      }
      teamStates.foldLeft(Evented(game)) { case (evtGame, (player, state)) =>
        evtGame.flatMap { game =>
          forPlayer(teamObjects, player, state, game.world).flatMap {
            case (world, newState) => Evented(
              game.copy(world = world, states = game.states + (player -> newState)),
              ActionsChangeEvt(player, newState.actions)
            )
          }
        }
      }
    }
  }
}

case class Game private (
  world: World, states: Game.States, objectives: Game.ObjectivesMap
) {
  import app.models.game.Game.Withs._

  private[this] def fromWorld(w: Evented[World]) = w.map(updated)

  def gameTurnStarted(implicit log: LoggingAdapter) = world.gameTurnStarted |> fromWorld
  def gameTurnFinished(implicit log: LoggingAdapter) = world.gameTurnFinished |> fromWorld
  def teamTurnStarted(team: Team)(implicit log: LoggingAdapter) =
    checkObjectivesSafe(team) {
      world.teamTurnStarted(team) |> fromWorld |>
      Game.recalculatePlayerStatesOnTeamTurnStart(team)
    }
  def teamTurnFinished(team: Team)(implicit log: LoggingAdapter)
  : Evented[Winner \/ Game] = (
    world.teamTurnFinished(team) |> fromWorld |> Game.doAutoSpecials(team) |>
    checkObjectivesSafe(team)
  ).flatMap(checkObjectivesForWinner)

  private[this] def checkObjectivesForWinner(game: Game): Evented[Winner \/ Game] = {
    val winningTeamOpt = world.teams.find { team =>
      val remaining = game.remainingObjectives(team)
      remaining.someCompleted
    }
    winningTeamOpt.fold2(
      Evented(game.rightZ),
      winnerTeam => Evented(Winner(winnerTeam).leftZ, GameWonEvt(winnerTeam))
    )
  }

  private[this] def teamStates(team: Team) = states.view.filter(_._1.team === team)

  def actionsLeftFor(team: Team) = teamStates(team).map(_._2.actions).sum
  /* Checks if all players have sent the turn ended flag. */
  def allPlayersTurnEnded(team: Team) = teamStates(team).forall(!_._2.activity.canAct)
  def otherTeamsConceded(team: Team): Boolean =
    states.filter(_._1.team =/= team).forall(_._2.activity === GamePlayerState.Conceded)

  def isJoined(human: Human)(implicit log: LoggingAdapter) =
    states.contains(human)

//  def join(human: Human, startingResources: Resources)
//  (implicit log: LoggingAdapter) = {
//    def evt(newState: GamePlayerState) = Evented(
//      updated(world, human -> newState),
//      JoinEvt(
//        human,
//        Some(HumanState(startingResources, world.populationFor(human), newState))
//      )
//    )
//
//    checkObjectives(human.team) {
//    states.get(human).fold2(
//      evt(Game.startingState(world, human)).map { g =>
//        if (g.world.resourcesMap.contains(human)) Evented(g).right
//        else g.world.addResources(human, startingResources).right.map(_.map(g.updated))
//      }.extractFlatten,
//      state => s"$human is already in the game!".left
//    ) }
//  }
//
//  def leave(human: Human)(implicit log: LoggingAdapter) =
//    checkObjectives(human.team) {
//    withState(human) { state =>
//      Evented(updated(states - human), Vector(LeaveEvt(human))).right
//    } }

  def warp(
    human: Human, position: Vect2, warpable: WarpableCompanion.Some
  )(implicit log: LoggingAdapter): Game.Result =
    checkObjectives(human.team) {
    withState(human) { state =>
    withActions(human, Warpable.ActionsNeeded, state) { state =>
    withWarpable(human, state.canWarp, warpable, world) { evtWorld =>
    withWarpZone(human, position) {
      evtWorld.map { warpable.warpW(_, human, position).right.map { _.map {
        updated(_, human -> state)
      } } }.extract.right.map(_.flatten)
    } } } } }

  def move(
    human: Human, id: WObject.Id, to: NonEmptyVector[Vect2]
  )(implicit log: LoggingAdapter): Game.Result =
    checkObjectives(human.team) {
    withMoveObj(human, id) { obj =>
    withVisibility(human, to.v.last) {
      obj.moveTo(world, to).right.map { _.map { case (w, _) =>
        updated(w)
      } }
    } } }

  def attack(
    human: Human, id: WObject.Id, targetId: WObject.Id
  )(implicit log: LoggingAdapter): Game.Result =
    checkObjectives(human.team) {
    withAttackObj(human, id) { obj =>
    withTargetObj(human, targetId) { targetObj =>
    withVisibility(human, targetObj) {
      obj.attackW(targetObj, world).right.map { _.map { world =>
        updated(world)
      } }
    } } } }

  def moveAttack(
    human: Human, id: Id, path: NonEmptyVector[Vect2], targetId: Id
  )(implicit log: LoggingAdapter) = {
    checkObjectives(human.team) {
    move(human, id, path).right.flatMap { evtMovedGame =>
      // The object might get killed after the move.
      if (evtMovedGame.value.world.objects.contains(id)) {
        evtMovedGame.value.attack(human, id, targetId).right.map { evtAttackedGame =>
          evtMovedGame.events ++: evtAttackedGame
        }
      }
      else evtMovedGame.right
    } }
  }

  def special(
    human: Human, id: WObject.Id
  )(implicit log: LoggingAdapter): Game.Result =
    checkObjectives(human.team) {
    withState(human) { state =>
    withSpecialObj(human, id) {
    withSpecialAction(human, state) { obj => state =>
      obj.special(world, human).right.map(_.map(world => updated(world, human -> state)))
    } } } }

  def endTurn(human: Human)(implicit log: LoggingAdapter): Game.Result =
    checkObjectives(human.team) {
    withState(human) { state =>
      if (state.activity =/= GamePlayerState.WaitingForTurnEnd)
        s"Can't end turn: $human has state $state!".left
      else {
        Evented(
          updated(world, human -> state.onTurnEnd),
          TurnEndedChangeEvt(human, turnEnded = true)
        ).right
      }
    } }

  def concede(human: Human)(implicit log: LoggingAdapter): Game.Result =
    checkObjectives(human.team) {
    withState(human) { state =>
      if (state.activity === GamePlayerState.Conceded)
        s"Can't concede: $human has already conceded!".left
      else {
        Evented(
          updated(world, human -> GamePlayerState.conceded),
          TurnEndedChangeEvt(human, turnEnded = true)
        ).right
      }
    } }

  def movementFor(obj: Movable): Vector[Path] =
    Pathfinding.movement(
      obj, world.bounds, world.objects
    ).filter(_.vects.forall(world.visibilityMap.isVisible(obj.owner, _)))

  def visibleBy(owner: Owner) =
    copy(world = world.visibleBy(owner), states = states.filterKeys(_.isFriendOf(owner)))

  def remainingObjectives(team: Team) = {
    val teamObjectives = objectives.getOrElse(team, Objectives.empty)
    teamObjectives.remaining(this, team)
  }

  def updated(world: World): Game = copy(world = world)
  def updated(states: States): Game = copy(states = states)
  def updated(world: World, human: (Human, GamePlayerState)): Game =
    copy(world = world, states = states + human)

  private[this] def checkObjectives(team: Team)(f: Game.Result): Game.Result = {
    val currentObjectives = remainingObjectives(team)
    f.right.map(_.flatMap { game =>
      val newObjectives = game.remainingObjectives(team)
      Evented(
        game,
        if (currentObjectives =/= newObjectives)
          Vector(ObjectivesUpdatedEvt(team, newObjectives))
        else Vector.empty
      )
    })
  }

  private[this] def checkObjectivesSafe(team: Team)(f: Evented[Game]): Evented[Game] =
    checkObjectives(team)(f.right).right.get

  private[this] def withState(human: Human)(f: GamePlayerState => Game.Result) =
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
    world.objects.getCT[A](id).fold2(s"Cannot find object with id $id".left, f)
  }

  private[this] def withCheckedObj[A <: OwnedObj](
    id: WObject.Id, f: ObjFn[A]
  )(checker: A => Option[String])(implicit log: LoggingAdapter, ct: ClassTag[A])
  : Game.Result = {
    withObj[A](id) { obj =>
      checker(obj).fold2(
        f(obj),
        err => {
          log.info(s"$obj does not pass checker of type ${ct.runtimeClass}}: $err")
          s"Obj $id does not pass the checker: $err".left
        }
      )
    }
  }

  private[this] def withMoveObj(human: Human, id: WObject.Id)(
    f: ObjFn[OwnedObj with Movable]
  )(implicit log: LoggingAdapter): Game.Result = withCheckedObj(id, f) { obj =>
    (!Game.canMove(human, obj)).opt(s"$human can't move $obj")
  }

  private[this] def withAttackObj(human: Human, id: WObject.Id)(
    f: ObjFn[OwnedObj with Fighter]
  )(implicit log: LoggingAdapter): Game.Result = withCheckedObj(id, f) { obj =>
    (!Game.canAttack(human, obj)).opt(s"$human can't attack with $obj")
  }

  private[this] def withSpecialObj(human: Human, id: WObject.Id)(
    f: ObjFn[OwnedObj with SpecialAction]
  )(implicit log: LoggingAdapter): Game.Result = withCheckedObj(id, f) { obj =>
    (!obj.canDoSpecial(human)).opt(s"$human can't do special with $obj")
  }

  private[this] def withTargetObj(human: Owner, id: WObject.Id)(
    f: ObjFn[OwnedObj]
  )(implicit log: LoggingAdapter): Game.Result = withObj[OwnedObj](id) { obj =>
    if (human.isEnemyOf(obj.owner)) f(obj)
    else s"Cannot target friendly $obj for attack!".left
  }
}
