package app.algorithms.behaviour_trees.leaves

import akka.event.LoggingAdapter
import app.algorithms.Pathfinding
import app.algorithms.Pathfinding.SearchRes
import app.algorithms.behaviour_trees._
import app.algorithms.behaviour_trees.BehaviourTree.NodeResult
import app.models.game.ai.SingleMindAI
import app.models.game.world.{WUnit, Fighter, OwnedObj, World}
import implicits._

sealed trait TargetFindMode
object TargetFindMode {
  case object Closest extends TargetFindMode
  case object MostDmg extends TargetFindMode
}

case class ClearTarget[State <: AttackTargetBlackboard[State]]()
extends LeafNode[State] {
  def run(state: State)(implicit log: LoggingAdapter) =
    (NodeResult.success, state.attackTargetLens.set(state, None))
}

case class FindVisibleTarget[
  S <: (
    AttackTargetBlackboard[S] with GameBlackboard[S] with
    FighterUnitContext[S]
  )
](
  mode: TargetFindMode=TargetFindMode.MostDmg
) extends LeafNode[S] with NeedsFighterUnitContext[S]
{
  def run(state: S)(implicit log: LoggingAdapter) = {
    withUnchangedUnit(state) { (state, unit) =>
      if (unit.hasAttacksLeft) {

      }

      val evtGame = state.gameLens.get(state)
      val world = evtGame.value.world
      val visibleTargets =
        world.objects.view.
          collect { case o: OwnedObj => o }.
          filter(o => o.isEnemy(unit) && unit.sees(o)).
          toSeq
      val target = findTarget(world, visibleTargets, unit)

      target.fold2(
        (NodeResult.Failure, state),
        _ => (NodeResult.Success, state.attackTargetLens.set(state, target))
      )
    }
  }

  def findTarget(
    world: World, targets: Iterable[OwnedObj], unit: FUnit
  )(implicit log: LoggingAdapter): Option[SearchRes[OwnedObj]] = {
    if (targets.isEmpty) return None

    val attackableTargets =
      Pathfinding.attackSearch(unit, targets, world.bounds, world.objects)(_.bounds)

    if (attackableTargets.isEmpty) {
      log.debug(
        s"findTarget: no attackable targets from {} for {}", targets, unit
      )
      None
    }
    else {
      val target = attackableTargets.reduce(
        SingleMindAI.atkOrdRndLt(SingleMindAI.AttackSearchResOrdering(unit))
      )
      log.debug(
        s"findTarget: attackable target = {} from {} for {}",
        target, targets, unit
      )
      Some(target)
    }
  }
}