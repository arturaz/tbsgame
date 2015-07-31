package app.algorithms.behaviour_trees

import akka.event.LoggingAdapter
import app.algorithms.Pathfinding.SearchRes
import app.algorithms.behaviour_trees.BehaviourTree.NodeResult
import app.models.game.{Player, Game}
import app.models.game.events.Evented
import app.models.game.world._
import monocle.Lenser
import implicits._

import scala.util.control.NonFatal

object AI {
  trait AI extends BehaviourTreeNodes {
    val ai: BehaviourTree[St, _]
    def apply(state: St) = ai.run(state)
  }

  case class BotAI(player: Player)(implicit loggingAdapter: LoggingAdapter) extends AI {
    /*
TODO:
If enough resources for extractor && drone ready
	Find place for extractor
		Sort visible planets by distance from drone * C1 + max resource rate * C2 + max resources left * C3
	If found
		Move drone to planet && build extractor
	Else
		Move drone to the nearest dark region
Convert 1 action to resource
     */

    // If enough resources for drone && no drones available
    // - Build drone in random spot around spawner
    def BuildDroneInRandomPosition(player: Player) = Sequence_(
      EnoughResourcesFor_?(player, DroneStats),
      ! OwnsWObject_?(player) { case _: Drone => true },
      WarpInInRandomPosition(player, DroneStats)
    )

    override val ai = Selector(
      BuildDroneInRandomPosition(player)
    )
  }

//  object SingleMind extends AI {
//    type State = Blackboard
//    val ai = Sequence[Blackboard](
//      FindVisibleTarget(),
//      MoveTowardsAttackTarget(),
//      AttackTarget()
//    )
//    val ai = ???

    /*
    for {
      target <- fUnit.findTarget
      _ <- fUnit.moveTowards(target)
      _ <- fUnit.attackTarget(target)
    } yield ()
     */

//    val lenser = Lenser[Blackboard]
//    val unitLens = lenser(_.unit)
//    val gameLens = lenser(_.game)
//    val attackTargetLens = lenser(_.attackTarget)
//
//    case class Blackboard(
//      unit: Option[FUnit],
//      game: Evented[Game],
//      attackTarget: Option[SearchRes[OwnedObj]]
//    )
//    extends GameBlackboard[Blackboard]
//    with AttackTargetBlackboard[Blackboard]
//    with FighterUnitContext[Blackboard]
//    {
//      def unitLens = SingleMind.unitLens
//      def gameLens = SingleMind.gameLens
//      def attackTargetLens = SingleMind.attackTargetLens
//    }
//  }
}
