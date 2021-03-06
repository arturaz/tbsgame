//package app.algorithms.behaviour_trees.leaves
//
//import akka.event.LoggingAdapter
//import app.algorithms.Pathfinding.SearchRes
//import app.algorithms.behaviour_trees.BehaviourTree.NodeResult
//import app.algorithms.behaviour_trees._
//import app.models.game.events.Evented
//import app.models.game.world.{World, OwnedObj}
//import app.models.game.world.WObject._
//import implicits._
//
//trait NeedsFighterUnitContext[S <: FighterUnitContext[S]] {
//  def withUnit
//    (state: S)
//    (f: FUnit => (NodeResult, S, Option[FUnit]))
//    (implicit log: LoggingAdapter)
//  = {
//    state.unitLens.get(state).fold2(
//      (
//        NodeResult.error(
//          s"$this needs fighter unit context, but it was not found in $state!"
//        ),
//        state
//      ),
//      unit => {
//        val (res, newState, newUnitOpt) = f(unit)
//        (res, newState.unitLens.set(newState, newUnitOpt))
//      }
//    )
//  }
//
//  def withUnchangedUnit
//    (state: S)
//    (f: FUnit => BehaviourTree[S]#Run)
//    (implicit log: LoggingAdapter)
//  = {
//    withUnit(state) { unit =>
//      val (res, newState) = f(unit)
//      (res, newState, Some(unit))
//    }
//  }
//}
//
//trait NeedsAttackTarget[S <: AttackTargetBlackboard[S]] {
//  def withAttackTarget
//    (state: S)
//    (f: SearchRes[OwnedObj] => BehaviourTree[S]#Run)
//    (implicit log: LoggingAdapter)
//  = {
//    state.attackTargetLens.get(state).fold2(
//      (NodeResult.error(s"$state did not have attack target!"), state),
//      f
//    )
//  }
//}
//
//trait GameUpdate[S <: GameBlackboard[S]] {
//  def updateGameOpt(
//    state: S, update: WorldObjOptUpdate[FUnit]
//  ): S = updateGameWorld(state, update.map(_._1))
//
//  def updateGame(
//    state: S, update: WorldObjUpdate[FUnit]
//  ): S = updateGameWorld(state, update.map(_._1))
//
//  def updateGameWorld(
//    state: S, update: Evented[World]
//  ): S = {
//    state.gameLens.modify(state, currentGame => {
//      currentGame.flatMap(game => update.map(game.updated))
//    })
//  }
//}