package app.algorithms.behaviour_trees

import akka.event.LoggingAdapter
import app.algorithms.behaviour_trees.BehaviourTree.NodeResult

object BehaviourTree {
  sealed trait NodeResult
  object NodeResult {
    /* returned when a criterion has been met by a condition node or an action node has
       been completed successfully */
    case object Success extends NodeResult
    /* returned when a criterion has not been met by a condition node or an action node
       could not finish its execution for any reason */
    case object Failure extends NodeResult
    /* returned when an action node has been initialized but is still waiting the its
       resolution */
    case object Running extends NodeResult
    /* returned when some unexpected error happened in the tree, probably by a
       programming error (trying to verify an undefined variable). Its use depends on the
       final implementation of the leaf nodes */
    case class Error(error: String) extends NodeResult

    @inline def success: NodeResult = Success
    @inline def failure: NodeResult = Failure
    @inline def running: NodeResult = Running
    @inline def error(str: String): NodeResult = Error(str)
  }
}

trait BehaviourTree[State] {
  type BT = BehaviourTree[State]
  type Run = (NodeResult, State)

  def run(state: State)(implicit log: LoggingAdapter): Run
}

trait CompositeNode[State] extends BehaviourTree[State] {
  val children: Seq[BehaviourTree[State]]
}

trait DecoratorNode[State] extends BehaviourTree[State] {
  val child: BehaviourTree[State]
}

trait LeafNode[State] extends BehaviourTree[State]

case class RootNode[State](
  child: BehaviourTree[State]
) extends BehaviourTree[State] {
  def run(state: State)(implicit log: LoggingAdapter) = child.run(state)
}