//package app.algorithms.behaviour_trees
//
//import akka.event.LoggingAdapter
//import app.algorithms.behaviour_trees.BehaviourTree.NodeResult
//
//import scala.annotation.tailrec
//
//case class Inverter[State](
//  child: BehaviourTree[State]
//) extends DecoratorNode[State] {
//  override def run(state: State)(implicit log: LoggingAdapter) =
//    child.run(state) match {
//      case (NodeResult.Success, st) => (NodeResult.failure, st)
//      case (NodeResult.Failure, st) => (NodeResult.success, st)
//      case other => other
//    }
//}
//
//case class Succeeder[State](
//  child: BehaviourTree[State]
//) extends DecoratorNode[State] {
//  override def run(state: State)(implicit log: LoggingAdapter) =
//    (NodeResult.success, child.run(state)._2)
//}
//
//case class Failer[State](
//  child: BehaviourTree[State]
//) extends DecoratorNode[State] {
//  override def run(state: State)(implicit log: LoggingAdapter) =
//    (NodeResult.failure, child.run(state)._2)
//}
//
///* Repeater decorator sends the tick signal to its child every time that its child
//   returns a SUCCESS or FAILURE value, or when this decorator receives the tick. */
//case class Repeater[State](
//  child: BehaviourTree[State], maxRepeats: Int=0
//) extends DecoratorNode[State] {
//  override def run(state: State)(implicit log: LoggingAdapter) = {
//    @tailrec def rec(idx: Int, state: State): Run = {
//      if (maxRepeats < 0 || idx >= maxRepeats) (NodeResult.Success, state)
//      else child.run(state) match {
//        case (NodeResult.Success | NodeResult.Failure, st) => rec(idx + 1, st)
//        case other => other
//      }
//    }
//
//    rec(0, state)
//  }
//}
//
//case class RepeatUntilFail[State](
//  child: BehaviourTree[State]
//) extends DecoratorNode[State] {
//  @tailrec final override def run(state: State)(implicit log: LoggingAdapter) = {
//    child.run(state) match {
//      case (NodeResult.Failure, st) => (NodeResult.success, st)
//      case (NodeResult.Success, st) => run(st)
//      case other => other
//    }
//  }
//}