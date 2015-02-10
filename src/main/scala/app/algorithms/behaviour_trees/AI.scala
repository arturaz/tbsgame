package app.algorithms.behaviour_trees

import akka.event.LoggingAdapter
import app.algorithms.Pathfinding.SearchRes
import app.algorithms.behaviour_trees.leaves.{AttackTarget, FindVisibleTarget, MoveTowardsAttackTarget}
import app.models.game.Game
import app.models.game.events.Evented
import app.models.game.world.{Fighter, OwnedObj}
import monocle.Lenser

object AI {
  trait AI {
    type State
    val ai: BehaviourTree[State]
    def apply(state: State)(implicit log: LoggingAdapter) = ai.run(state)
  }

  object SingleMind extends AI {
    val ai = Sequence[Blackboard](
      FindVisibleTarget(),
      MoveTowardsAttackTarget(),
      AttackTarget()
    )

    val lenser = Lenser[Blackboard]
    val unitLens = lenser(_.unit)
    val gameLens = lenser(_.game)
    val attackTargetLens = lenser(_.attackTarget)

    type State = Blackboard
    case class Blackboard(
      unit: Option[FUnit],
      game: Evented[Game],
      attackTarget: Option[SearchRes[OwnedObj]]
    )
    extends GameBlackboard[Blackboard]
    with AttackTargetBlackboard[Blackboard]
    with FighterUnitContext[Blackboard]
    {
      def unitLens = SingleMind.unitLens
      def gameLens = SingleMind.gameLens
      def attackTargetLens = SingleMind.attackTargetLens
    }
  }
}
