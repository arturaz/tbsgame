package app.models.game

import app.models.game.world.props.Asteroid
import app.models.game.world.{World, OwnedObj, VPS, Resources}
import implicits._
import utils.IntValueClass

sealed trait Objective {
  type Remaining <: IntValueClass[Remaining]
  def remainingZero: Remaining

  def remaining(game: Game, team: Team) = remainingImpl(game, team) max remainingZero
  protected def remainingImpl(game: Game, team: Team): Remaining

  def isCompleted(remaining: Remaining): Boolean = remaining.isZero
  def isCompleted(game: Game, team: Team): Boolean = remaining(game, team) |> isCompleted
}

object Objective {
  object GatherResources {
    /* Gather either `resources` of `topPercentage`% of total world resources, whichever
       is less. */
    def apply(
      world: World, resources: Resources, topPercentage: Percentage
    ): GatherResources = {
      val total = world.objects.collect { case a: Asteroid => a.resources }.sum
      val top = Resources((total.value * topPercentage.value).floor.toInt)
      apply(resources min top)
    }
  }
  case class GatherResources(resources: Resources) extends Objective {
    type Remaining = Resources
    def remainingZero = Resources(0)

    def remainingImpl(game: Game, team: Team) =
      resources -
      game.world.resourcesMap.filterKeys(_.team === team).map(_._2).sum

    override def isCompleted(remaining: Remaining) = remaining.isZero
  }

  case class CollectVPs(vps: VPS) extends Objective {
    type Remaining = VPS
    def remainingZero = VPS(0)

    def remainingImpl(game: Game, team: Team) =
      vps -
      game.world.vpsMap.filterKeys(_ === team).map(_._2).sum

    override def isCompleted(remaining: Remaining) = remaining <= VPS(0)
  }

  case object DestroyAllCriticalObjects extends Objective {
    type Remaining = ObjectCount
    def remainingZero = ObjectCount(0)

    def remainingImpl(game: Game, team: Team) =
      ObjectCount(game.world.objects.count {
        case oo: OwnedObj if oo.companion.isCritical && oo.owner.isEnemyOf(team) => true
        case _ => false
      })

    override def isCompleted(remaining: Remaining) = remaining.isZero
  }
}

object Objectives {
  val empty = apply()
}
case class Objectives(
  gatherResources: Option[Objective.GatherResources]=None,
  collectVps: Option[Objective.CollectVPs]=None,
  destroyAllCriticalObjects: Option[Objective.DestroyAllCriticalObjects.type]=None
) {
  def remaining(game: Game, team: Team) = RemainingObjectives(
    gatherResources.map(_.remaining(game, team)),
    collectVps.map(_.remaining(game, team)),
    destroyAllCriticalObjects.map(_.remaining(game, team))
  )
}

object RemainingObjectives {
  private def isCompleted(ivc: IntValueClass[_]): Boolean = ivc.value <= 0
  private def isCompleted(ivcOpt: Option[IntValueClass[_]]): Boolean =
    ivcOpt.exists(isCompleted)
}
case class RemainingObjectives(
  gatherResources: Option[Resources]=None,
  collectVps: Option[VPS]=None,
  destroyAllCriticalObjects: Option[ObjectCount]=None
) {
  import RemainingObjectives._

  val someCompleted =
    isCompleted(gatherResources) || isCompleted(collectVps) ||
    isCompleted(destroyAllCriticalObjects)
  def noneCompleted = ! someCompleted
}