//package app.algorithms.behaviour_trees.leaves
//
//import akka.event.LoggingAdapter
//import app.algorithms.behaviour_trees.BehaviourTree.NodeResult
//import app.algorithms.behaviour_trees.{PlayerContext, LeafNode, GameBlackboard}
//import app.models.game.world.WObject
//
//case class OwnsWObject[S <: GameBlackboard[S] with PlayerContext[S]](
//  predicate: PartialFunction[WObject, Boolean]
//) extends LeafNode[S] {
//  override def run(state: S)(implicit log: LoggingAdapter) = {
//    val success =
//      state.world.objects.forOwner(state.player).
//      collectFirst(predicate).getOrElse(false)
//    (NodeResult.sOrF(success), state)
//  }
//}