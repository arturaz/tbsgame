package app.models.game

import akka.event.LoggingAdapter
import app.algorithms.behaviour_trees.AI.BotAI
import app.algorithms.behaviour_trees.BehaviourTree.NodeResult
import app.models.game.Game.States
import app.models.game.GamePlayerState.CanWarp
import app.models.game.events._
import app.models.game.world._
import implicits._
import monocle.Lenser
import monocle.syntax._
import utils.data.NonEmptyVector
import app.models.game.world.Ops._

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scalaz._, Scalaz._

object Game {
  private[this] def lenser = Lenser[Game]
  val worldL = lenser(_.world)
  val statesL = lenser(_.states)

  type ResultT[A] = String \/ Evented[A]
  type Result = ResultT[Game]
  type ResultOrWinner = ResultT[Winner \/ Game]
  type States = Map[Player, GamePlayerState]
  type ObjectivesMap = Map[Team, Objectives]

  object StartingPlayer {
    val DefaultWarpSet: GamePlayerState.CanWarp = Set(
      ExtractorStats,
      WarpLinkerStats,
      LaserTowerStats,
      PopulationTowerStats,
      ActionTowerStats,

      ScoutStats,
      CorvetteStats,
      RocketFrigateStats,
      WarpPrismStats,
      GunshipStats
    )
  }
  case class StartingPlayer(
    player: Player, resources: Resources,
    canWarp: GamePlayerState.CanWarp=StartingPlayer.DefaultWarpSet
  )

  def apply(
    world: World, starting: Set[StartingPlayer], objectives: ObjectivesMap
  ): String \/ Game = {
    val startingPlayersWarpMap: Map[Player, CanWarp] =
      starting.map(sp => sp.player -> sp.canWarp)(collection.breakOut)

    starting.foldLeft(Evented(world).right[String]) {
      case (left: -\/[_], _) => left
      case (\/-(eWorld), data) =>
        eWorld.map(_.addResources(data.player, data.resources)).extractFlatten
    }.map { evtWorld =>
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
      if (player.isHuman) GamePlayerState.Active else GamePlayerState.WaitingForNextRound,
      canWarp
    )

  def startingStates(
    world: World, players: Map[Player, GamePlayerState.CanWarp]
  ): States = players.map { case (player, canWarp) =>
    player -> startingState(world, player, canWarp)
  }

  def canMove(player: Player, obj: Movable) = obj.owner === player
  def canAttack(player: Player, obj: Fighter) = player.isFriendOf(obj.owner)

  private object Withs {
    def withActions[A](player: Player, actionsNeeded: Actions, state: GamePlayerState)(
      f: GamePlayerState => Game.ResultT[A]
    ): Game.ResultT[A] = {
      if (state.actions < actionsNeeded)
        s"Not enough actions: needed $actionsNeeded, had $state".left
      else {
        val newState =
          state |-> GamePlayerState.actions modify (_ - actionsNeeded)
        val events =
          if (actionsNeeded > Actions(0))
            Vector(ActionsChangeEvt(player, newState.actions))
          else
            Vector.empty
        f(newState).map(events ++: _)
      }
    }

    def withSpecialAction[A <: SpecialAction, B](
      player: Player, state: GamePlayerState
    )(f: A => GamePlayerState => Game.ResultT[B])(obj: A): Game.ResultT[B] =
      withActions(player, obj.stats.specialActionsNeeded, state)(f(obj))

    def withResources[A](
      player: Player, resourcesNeeded: Resources, world: World
    )(f: Evented[World] => Game.ResultT[A]): Game.ResultT[A] =
      world.subResources(player, resourcesNeeded).flatMap(f)

    def withPopulation[A](
      player: Player, populationNeeded: Population, world: World
    )(f: Evented[World] => Game.ResultT[A]): Game.ResultT[A] = {
      val population = world.populationFor(player)
      if (populationNeeded.isZero || population.left >= populationNeeded) f(Evented(
        world,
        PopulationChangeEvt(player, population.withValue(_ + populationNeeded))
      ))
      else
        s"Needed $populationNeeded, but $player only had $population!".left
    }

    def withAbilityToWarp[A](
      player: Player, canWarp: GamePlayerState.CanWarp, warpable: WarpableCompanion.Some
    )(f: => Game.ResultT[A]): Game.ResultT[A] = {
      if (canWarp.contains(warpable)) f
      else s"$player cannot warp $warpable! (allowed=$canWarp)".left
    }

    def withWarpable[A](
      player: Player, canWarp: GamePlayerState.CanWarp, warpable: WarpableCompanion.Some,
      world: World
    )(f: Evented[World] => Game.ResultT[A]): Game.ResultT[A] = {
      withResources(player, warpable.cost, world) { evtWorld =>
      withPopulation(player, warpable.populationCost, evtWorld.value) { evtWorld2 =>
      withAbilityToWarp(player, canWarp, warpable) {
        f(evtWorld.events ++: evtWorld2)
      } } }
    }
  }

  private def recalculatePlayerStatesOnRoundStart
  (g: Evented[Game]): Evented[Game] =
    g.flatMap { game =>
      game.states.foldLeft(Evented(game.states)) { case (e, (player, state)) =>
        val newState = state.onRoundStart(player, game.world.actionsFor(player))
        if (state === newState) e
        else e.flatMap { curStates => Evented(
          curStates + (player -> newState),
          player.asHuman.fold2(
            Vector.empty,
            human => Vector(WaitingForRoundEndChangeEvt(human, newState.activity.canAct))
          ) :+ ActionsChangeEvt(player, newState.actions)
        ) }
      }.map(game.updated)
    }

  def doAutoSpecialIgnoreErrors(game: Game, player: Player)(implicit log: LoggingAdapter)
  : Evented[Game] = doAutoSpecial(game, player).fold(
    err => {
      log.error(
        "Game.doAutoSpecialIgnoreErrors failed for {}: {}", player, err
      )
      Evented(game)
    },
    identity
  )

  /* Do one auto special action for given player. */
  def doAutoSpecial(game: Game, player: Player)(implicit log: LoggingAdapter)
  : Game.Result = {
    def forPlayer(
      obj: AutoSpecial, player: Player, state: GamePlayerState,
      startingWorld: World
    ): String \/ Evented[(World, GamePlayerState)] = {
      def newState(actionsUsed: Actions) =
        state.copy(actions = state.actions - actionsUsed)

      obj.special(startingWorld, player).fold(
        err =>
          s"Failed to do auto special for $player on $obj with state $state: $err".left,
        _.map { world =>
          (world, newState(obj.stats.specialActionsNeeded))
        }.right
      )
    }

    val team = player.team
    val playerState = game.states(player)
    if (playerState.actions.isZero)
      s"Player actions is zero ($playerState), can't do auto special.".left
    else {
      val objOpt = game.world.objects.collectFirst {
        case obj: AutoSpecial
          if obj.owner.isFriendOf(team) && obj.canDoSpecial(player) => obj
      }
      objOpt.fold2(
        Evented(game).right,
        obj => forPlayer(obj, player, playerState, game.world).map(_.flatMap {
          case (world, newState) => Evented(
            game.copy(world = world, states = game.states + (player -> newState)),
            ActionsChangeEvt(player, newState.actions)
          )
        })
      )
    }
  }
}

case class Game private (
  world: World, states: Game.States, objectives: Game.ObjectivesMap
) {
  import app.models.game.Game.Withs._

  private[this] def fromWorld(w: Evented[World]) = w.map(updated)

  def roundStarted(implicit log: LoggingAdapter) =
    // TODO: runAI ?
    world.roundStarted |> fromWorld |>
    Game.recalculatePlayerStatesOnRoundStart
  def roundEnded(implicit log: LoggingAdapter) = world.roundEnded |> fromWorld

  private[this] def runAI(team: Team)(e: Evented[Game])(implicit log: LoggingAdapter) = {
    val scopedLog = log.prefixed("runAI|")
    val bots = e.value.world.bots.filter(_.team === team)
    bots.foldLeft(e) { case (fEvtWorld, bot) =>
      val ai = BotAI(bot)(scopedLog)
      @tailrec def rec(evtW: ai.St): ai.St = ai(fEvtWorld) match {
        case (st, NodeResult.Success(_)) => rec(st)
        case (st, NodeResult.Failure(reason)) =>
          scopedLog.info("AI failure: {}", reason)
          st
        case (st, NodeResult.Error(err)) =>
          scopedLog.error("AI error: {}", err)
          st
      }

      rec(fEvtWorld)
    }
  }

  private[this] def teamStates(team: Team) = states.view.filter(_._1.team === team)

  def allPlayersFinished = states.forall { case (player, state) =>
    (
      // TODO: fix bots later
      player.isBot || state.actions.isZero
    ) && !state.activity.canAct
  }

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
    player: Player, position: Vect2, warpable: WarpableCompanion.Some
  )(implicit log: LoggingAdapter): Game.ResultOrWinner =
    warpW[Warpable](player, position, warpable).map(_.map(_.map(_._1)))

  def warpW[A <: Warpable](
    player: Player, position: Vect2, warpable: WarpableCompanion.Of[A]
  )(implicit log: LoggingAdapter): Game.ResultT[Winner \/ (Game, Option[A])] =
    checkObjectives {
      val res =
        withState(player) { state =>
        withActions(player, Warpable.ActionsNeeded, state) { state =>
        withWarpable(player, state.canWarp, warpable, world) { evtWorld =>
        withWarpZone(player, position) {
          evtWorld.map { warpable.warp(_, player, position).map { _.map {
            case (world, optUnit) => (updated(world, player -> state), optUnit)
          } } }.extract.map(_.flatten)
        } } } }
      res
    } (_._1)

  def move(
    player: Player, id: WObject.Id, to: NonEmptyVector[Vect2]
  )(implicit log: LoggingAdapter): Game.ResultOrWinner =
    checkObjectives {
      withState(player) { state =>
      withActions(player, Actions(1), state) { state =>
      withMoveObj(player, id) { obj =>
      withVisibility(player, to) {
        obj.moveTo(world, to).map { _.map { case (w, _) =>
          updated(w, player -> state)
        } }
      } } } }
    }

  def attack(
    player: Player, id: WObject.Id, target: Vect2 \/ WObject.Id
  )(implicit log: LoggingAdapter): Game.ResultOrWinner =
    checkObjectives {
      withAttackObj(player, id) { obj =>
        target.fold(
          targetPos => obj.attackPosW(targetPos, world).map(_.map(updated)),
          targetId =>
            withTargetObj(player, targetId) { targetObj =>
            withVisibility(player, targetObj) {
              obj.attack(targetObj, world).mapRes(t => updated(t._1))
            } }
        )
      }
    }

  def attack(
    player: Player, id: WObject.Id, targetId: WObject.Id
  )(implicit log: LoggingAdapter): Game.ResultOrWinner =
    attack(player, id, targetId.right)

  def attackPosition(
    player: Player, id: WObject.Id, targetPos: Vect2
  )(implicit log: LoggingAdapter): Game.ResultOrWinner =
    attack(player, id, -\/(targetPos))

  def moveAttack(
    player: Player, id: WObject.Id, path: NonEmptyVector[Vect2],
    target: Vect2 \/ WObject.Id
  )(implicit log: LoggingAdapter): Game.ResultOrWinner =
    checkObjectives {
      withState(player) { state =>
      withActions(player, Actions(1), state) { state =>
      withMoveAttackObj(player, id) { obj =>
      withVisibility(player, path) {
        def moveAndAttack(
          attack: (OwnedObj with Movable with Fighter, World) => String \/ Evented[World]
        ) = {
          val evtFinal = obj.moveTo(world, path).flatMap {
            // We were destroyed after moving, do nothing
            case Evented((world, None), events) => Evented(world, events).right
            case Evented((world, Some(afterMoveObj)), events) =>
              attack(afterMoveObj, world).map { evtAfterAttack =>
                events ++: evtAfterAttack
              }
          }

          evtFinal.map { _.map { world => updated(world, player -> state) } }
        }

        target.fold(
          targetPos =>
            moveAndAttack(_.attackPosW(targetPos, _)),
          targetId =>
            withTargetObj(player, targetId) { targetObj =>
            withVisibility(player, targetObj) {
              moveAndAttack {
                _.attack(targetObj, _, Fighter.VisibilityCheck.Off).mapRes(_._1)
              }
            } }
        )
      } } } }
    }

  def turnTimeEnded(player: Player)(implicit log: LoggingAdapter): Game.ResultOrWinner =
    checkObjectives {
    withState(player) { state =>
      if (state.actions.isNotZero)
        Game.doAutoSpecial(this, player)
      else if (state.activity.canAct)
        toggleWaitForNextRoundWithoutCheckingObjectives(player)
      else
        Evented(this).right
    } }

  def special(
    player: Player, id: WObject.Id
  )(implicit log: LoggingAdapter): Game.ResultOrWinner =
    checkObjectives {
    withState(player) { state =>
    withSpecialObj(player, id) {
    withSpecialAction(player, state) { obj => state =>
      obj.special(world, player).map(_.map(world => updated(world, player -> state)))
    } } }
    }

  def toggleWaitForNextRound(player: Player)(implicit log: LoggingAdapter): Game.ResultOrWinner =
    checkObjectives { toggleWaitForNextRoundWithoutCheckingObjectives(player) }

  def toggleWaitForNextRoundWithoutCheckingObjectives(
    player: Player
  )(implicit log: LoggingAdapter): Game.Result =
    withState(player) { state =>
      val newState = state.toggleWaitForNextRound
      Evented(
        updated(world, player -> newState),
        Vector(WaitingForRoundEndChangeEvt(player, newState.activity.canAct))
      ).right
    }

  def concede(human: Human)(implicit log: LoggingAdapter): Game.ResultOrWinner =
    checkObjectives {
      val res =
        withState(human) { state =>
          if (state.activity === GamePlayerState.Conceded)
            s"Can't concede: $human has already conceded!".left
          else
            Evented(updated(world, human -> GamePlayerState.conceded)).right
        }
      res
    }

  def visibleBy(owner: Owner) =
    copy(world = world.visibleBy(owner), states = states.filterKeys(_.isFriendOf(owner)))

  def remainingObjectives(team: Team) = {
    val teamObjectives = objectives.getOrElse(team, Objectives.empty)
    teamObjectives.remaining(this, team)
  }

  def updated(world: World): Game = copy(world = world)
  def updated(states: States): Game = copy(states = states)
  def updated(world: World, player: (Player, GamePlayerState)): Game =
    copy(world = world, states = states + player)

  private[this] implicit def gameResultToCheckObjectivesResult[A](r: Game.ResultT[A])
  : Game.ResultT[Winner \/ A] = r.map(_.map(_.right))
  private[this] implicit def gameIdentity(game: Game): Game = game

  private[this] def checkObjectives[A](f: Game.ResultT[A])
  (implicit getGame: A => Game): Game.ResultT[Winner \/ A] = {
    def getObjectives(game: Game) =
      game.world.teams.map { team => team -> game.remainingObjectives(team) }.toMap

    val currentObjectives = getObjectives(this)
    f.map { evtA =>
      evtA.flatMap { a =>
        val game = getGame(a)
        val newObjectives = getObjectives(game)
        val differentObjectives = game.world.teams.foldLeft(
          Map.empty[Team, RemainingObjectives]
        ) { (m, team) =>
          val curObj = currentObjectives(team)
          val newObj = newObjectives(team)
          if (curObj === newObj) m else m updated (team, newObj)
        }
        val wonTeamOpt = differentObjectives.find(_._2.someCompleted)
        val objectiveUpdates = differentObjectives.map { case (team, obj) =>
          ObjectivesUpdatedEvt(team, obj)
        }.toVector

        wonTeamOpt.map(_._1).fold2(
          Evented(a.right, objectiveUpdates),
          team => Evented(Winner(team).left, objectiveUpdates :+ GameWonEvt(team))
        )
      }
    }
  }

  private def withState[A](player: Player)(f: GamePlayerState => Game.ResultT[A])
  : Game.ResultT[A] =
    states.get(player).fold2(s"No state for $player: $states".left, f)

  private[this] def withVisibility(
    player: Player, position: Vect2
  )(f: => Game.Result): Game.Result =
    if (world.isVisibleFor(player, position)) f
    else s"$player does not see $position".left

  private[this] def withVisibility(
    player: Player, path: NonEmptyVector[Vect2]
  )(f: => Game.Result): Game.Result = {
    val invisiblePositions = path.v.filterNot(world.isVisibleFor(player, _))
    if (invisiblePositions.isEmpty) f
    else s"$player does not see path positions $invisiblePositions".left
  }

  private[this] def withVisibility(
    player: Player, obj: OwnedObj
  )(f: => Game.Result): Game.Result =
    if (world.isVisiblePartial(player, obj.bounds)) f
    else s"$player does not see $obj".left

  private[this] def withWarpZone[A](
    player: Player, position: Vect2
  )(f: => Game.ResultT[A]): Game.ResultT[A] =
    if (world.isValidForWarp(player, position)) f
    else s"$player cannot warp into $position".left

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

  private[this] def withMoveObj(player: Player, id: WObject.Id)(
    f: ObjFn[OwnedObj with Movable]
  )(implicit log: LoggingAdapter): Game.Result = withCheckedObj(id, f) { obj =>
    (!Game.canMove(player, obj)).opt(s"$player can't move $obj")
  }

  private[this] def withAttackObj(player: Player, id: WObject.Id)(
    f: ObjFn[OwnedObj with Fighter]
  )(implicit log: LoggingAdapter): Game.Result = withCheckedObj(id, f) { obj =>
    (!Game.canAttack(player, obj)).opt(s"$player can't attack with $obj")
  }

  private[this] def withMoveAttackObj(player: Player, id: WObject.Id)(
    f: ObjFn[OwnedObj with Movable with Fighter]
  )(implicit log: LoggingAdapter): Game.Result = withCheckedObj(id, f) { obj =>
    (!Game.canMove(player, obj)).opt(s"$player can't move $obj") orElse
    (!Game.canAttack(player, obj)).opt(s"$player can't attack with $obj")
  }

  private[this] def withSpecialObj(player: Player, id: WObject.Id)(
    f: ObjFn[OwnedObj with SpecialAction]
  )(implicit log: LoggingAdapter): Game.Result = withCheckedObj(id, f) { obj =>
    (!obj.canDoSpecial(player)).opt(s"$player can't do special with $obj")
  }

  private[this] def withTargetObj(player: Player, id: WObject.Id)(
    f: ObjFn[OwnedObj]
  )(implicit log: LoggingAdapter): Game.Result = withObj[OwnedObj](id) { obj =>
    if (player.isEnemyOf(obj.owner)) f(obj)
    else s"Cannot target friendly $obj for attack!".left
  }
}
