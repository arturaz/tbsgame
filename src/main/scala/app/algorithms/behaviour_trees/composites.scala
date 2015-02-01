package app.algorithms.behaviour_trees

import akka.event.LoggingAdapter
import app.algorithms.behaviour_trees.BehaviourTree.NodeResult

/* Runs all children sequentialy until if find one that does not return failure. */
case class Selector[State](
  children: BehaviourTree[State]*
) extends CompositeNode[State] {
  override def run(state: State)(implicit log: LoggingAdapter): Run = {
    children.foldLeft((NodeResult.failure, state)) {
      case ((NodeResult.Failure, st), node) => node.run(st)
      case (result, _) => return result
    }
  }
}

/* Runs all children sequentially until it finds one that does not return success. */
case class Sequence[State](
  children: BehaviourTree[State]*
) extends CompositeNode[State] {
  override def run(state: State)(implicit log: LoggingAdapter): Run = {
    children.foldLeft((NodeResult.success, state)) {
      case ((NodeResult.Success, st), node) => node.run(st)
      case (result, _) => return result
    }
  }
}