//package app.algorithms.behaviour_trees
//
//import akka.event.LoggingAdapter
//import app.algorithms.Pathfinding.SearchRes
//import app.algorithms.behaviour_trees.BehaviourTree.NodeResult
//import app.models.game.Owner
//import app.models.game.events.Evented
//import app.models.game.world.WObject._
//import app.models.game.world.{OwnedObj, WObject, World}
//import implicits._
//import monocle._
//
//trait BTHelpers[S] { self: BehaviourTree[S] =>
//  def enemyObjects(world: World, owner: Owner) =
//    world.objects.collect {
//      case o: OwnedObj if o.owner.isEnemyOf(owner) => o
//    }
//
//  def distanceTo(enemies: Vector[OwnedObj], obj: WObject) = {
//    val center = obj.bounds.center
//    enemies.view.map(e => e.bounds.center.radialDistance(center)).min
//  }
//
//  def withUnit
//    (state: S)
//    (f: FUnit => (NodeResult, S, Option[FUnit]))
//    (implicit log: LoggingAdapter, ev: S <:< FighterUnitContext[S])
//  = {
//    state.get(_.unitLens).fold2(
//      (
//        NodeResult.error(
//          s"$this needs fighter unit context, but it was not found in $state!"
//        ),
//        state
//      ),
//      unit => {
//        val (res, newState, newUnitOpt) = f(unit)
//        (res, newState.set(_.unitLens, newUnitOpt))
//      }
//    )
//  }
//
//  def withUnchangedUnit
//    (state: S)
//    (f: FUnit => Run)
//    (implicit log: LoggingAdapter, ev: S <:< FighterUnitContext[S])
//  = {
//    withUnit(state) { unit =>
//      val (res, newState) = f(unit)
//      (res, newState, Some(unit))
//    }
//  }
//
//  def withAttackTarget
//    (state: S)
//    (f: SearchRes[OwnedObj] => BehaviourTree[S]#Run)
//    (implicit log: LoggingAdapter, ev: S <:< AttackTargetBlackboard[S])
//  = {
//    state.get(_.attackTargetLens).fold2(
//      (NodeResult.error(s"$state did not have attack target!"), state),
//      f
//    )
//  }
//
//  def updateGameOpt
//    (state: S, update: WorldObjOptUpdate[FUnit])
//    (implicit ev: S <:< GameBlackboard[S])
//  : S = updateGameWorld(state, update.map(_._1))
//
//  def updateGame
//    (state: S, update: WorldObjUpdate[FUnit])
//    (implicit ev: S <:< GameBlackboard[S])
//  : S = updateGameWorld(state, update.map(_._1))
//
//  def updateGameWorld
//    (state: S, update: Evented[World])
//    (implicit ev: S <:< GameBlackboard[S])
//  : S = {
//    state.modify(_.gameLens) { gameEvt =>
//      gameEvt.flatMap(game => update.map(game.updated))
//    }
//  }
//}
