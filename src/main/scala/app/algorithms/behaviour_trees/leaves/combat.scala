//package app.algorithms.behaviour_trees.leaves
//
//import akka.event.LoggingAdapter
//import app.algorithms.behaviour_trees.BehaviourTree.NodeResult
//import app.algorithms.behaviour_trees.{FUnit, FighterUnitContext, LeafNode, AttackTargetBlackboard}
//import app.models.game.world.Ops._
//
//case class AttackTarget[S <: (
//  AttackTargetBlackboard[S] with FighterUnitContext[S]
//)]()
//extends LeafNode[S]
//{
//  def run(state: S)(implicit log: LoggingAdapter) = {
//    withAttackTarget(state) { target =>
//      withUnit(state) { unit =>
//        val world = state.gameLens.get(state).value.world
//        unit.attackWS(target.value, world).fold(
//          err => (
//            NodeResult.error(s"Error while attacking: $state"), state, Some(unit)
//          ),
//          update => (
//            NodeResult.success, updateGameOpt(state, update),
//            Some(update.value._2.asInstanceOf[FUnit])
//          )
//        )
//      }
//    }
//  }
//}