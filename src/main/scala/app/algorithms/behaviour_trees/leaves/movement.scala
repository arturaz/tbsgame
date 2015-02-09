package app.algorithms.behaviour_trees.leaves

import akka.event.LoggingAdapter
import app.algorithms.Pathfinding.SearchRes
import app.algorithms.behaviour_trees.BehaviourTree.NodeResult
import app.algorithms.behaviour_trees.{FUnit, FighterUnitContext, LeafNode, AttackTargetBlackboard}
import app.models.game.world.OwnedObj
import app.models.game.world.WObject.WorldObjOptUpdate
import implicits._

sealed trait MoveMode
object MoveMode {
  /* Move whole path in 1 AI tick */
  case object Full extends MoveMode
  /* Only move one step and return running afterwards. */
  case object StepByStep extends MoveMode
}

case class MoveTowardsAttackTarget[S <: (
  AttackTargetBlackboard[S] with FighterUnitContext[S]
)](
  mode: MoveMode=MoveMode.StepByStep
)
extends LeafNode[S]
with NeedsFighterUnitContext[S]
with NeedsAttackTarget[S]
with GameUpdate[S]
{
  def run(state: S)(implicit log: LoggingAdapter) = {
    withAttackTarget(state) { (state, target) =>
      withUnit(state) { (state, unit) =>
        run(state, target, unit)
      }
    }
  }

  def run(
    state: S, target: SearchRes[OwnedObj], unit: FUnit
  )(implicit log: LoggingAdapter)
  : (NodeResult, S, Option[FUnit]) = {
    val evtGame = state.gameLens.get(state)
    val world = evtGame.value.world
    unit.moveTo(world, mode match {
      case MoveMode.Full => target.path
      case MoveMode.StepByStep => target.path.head
    }).fold(
      err => (
        NodeResult.Error(s"Error while moving $unit to $target: $err"),
        state,
        Some(unit)
      ),
      update => {
        val newUnitOpt = update.value._2
        (NodeResult.success, updateGameOpt(state, update), newUnitOpt)
      }
    )
  }
}

