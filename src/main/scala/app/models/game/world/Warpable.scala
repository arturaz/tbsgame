package app.models.game.world

import akka.event.LoggingAdapter
import app.models.game.events.{Evented, WarpEvt, WarpStateChangeEvt}
import app.models.game.world.buildings._
import app.models.game.world.units._
import app.models.game.{Actions, Player, Population}
import implicits._

import scala.language.{existentials, implicitConversions}
import scalaz._, Scalaz._

sealed trait WarpableGroup
object WarpableGroup {
  case object Building extends WarpableGroup
  case object Unit extends WarpableGroup
}

trait WarpableCompanion[+Self <: Warpable] { _: WObjectStats =>
  /* Create Warpable at given position. */
  protected def warpWOReactionImpl(
    world: World, owner: Player, position: Vect2
  ): String \/ Self

  private[this] def warpWOReaction(
    world: World, owner: Player, position: Vect2, checkVisibility: Boolean
  ): String \/ Self = {
    val b = bounds(position)
    if (!checkVisibility || world.isVisibleFull(owner, b))
      warpWOReactionImpl(world, owner, position)
    else s"$b is not fully visible for $owner".left
  }

  /* Warp with reactions applied. */
  def warp(
    world: World, player: Player, position: Vect2, checkVisibility: Boolean = true
  )(implicit log: LoggingAdapter): String \/ WObject.WorldObjOptUpdate[Self] =
    warpWOReaction(world, player, position, checkVisibility).map { warpedIn =>
      World.revealObjects(
        player.team, WarpEvt(world.visibilityMap, warpedIn) +: world.add(warpedIn)
      ).flatMap(_.reactTo(warpedIn))
    }

  def warpW(
    world: World, player: Player, position: Vect2, checkVisibility: Boolean = true
  )(implicit log: LoggingAdapter): String \/ Evented[World] =
    warp(world, player, position, checkVisibility).map { _.map(_._1) }
}

trait EmptySpaceWarpableCompanion[Self <: Warpable] extends WarpableCompanion[Self] {
_: WObjectStats =>
  def warp(owner: Player, position: Vect2): Self

  override def warpWOReactionImpl(
    world: World, owner: Player, position: Vect2
  ): String \/ Self = {
    val b = bounds(position)
    if (! world.canWarp(b)) s"Can't warp in because $b is taken in $world".left
    else warp(owner, position).right
  }
}

object WarpableCompanion {
  type Of[A <: Warpable] = WarpableStats with WarpableCompanion[A]
  type Some = Of[Warpable]
}

trait WarpableStatsImpl { _: WarpableStats =>
  val InitialWarpState = WarpTime(0)
  val warpTime: WarpTime
  val cost: Resources
  val group: WarpableGroup
  val needsWarpZoneToWarp = true
}

trait WarpableImpl extends OwnedObjImpl {
  val warpState: WarpTime
  type Stats <: WarpableStats

  override def isWarpingIn = warpState < stats.warpTime
  override def goingToWarpInNextTurn = warpState + WarpTime(1) === stats.warpTime
  //  override def destroyReward = Some(Resources(
//    (companion.cost.value * Random.double(0.1, 1f / 3)).round.toInt
//  ))
}

trait WarpableOps[Self <: Warpable] extends OwnedObjOps[Self] {
  def setWarpState(newState: WarpTime): Self
  def nextWarpState(world: World): Evented[Self] =
    if (
      self.isWarpedIn || (
        self.stats.needsWarpZoneToWarp &&
        !world.isValidForWarp(self.owner, self.position)
      )
    ) Evented(self)
    else {

      val newSelf = setWarpState(self.warpState + WarpTime(1))
      Evented(newSelf, Vector(WarpStateChangeEvt(world.visibilityMap, newSelf)))
    }

  final def warpableTeamTurnStarted(world: World)(implicit log: LoggingAdapter) =
    WObject.selfEventedUpdate(world, self, nextWarpState(world))
}

trait ToWarpableOps {
  implicit def toWarpableOps[Self <: Warpable](a: Self): WarpableOps[Self] =
    (((a: Warpable) match {
      /* Buildings */

      case o: Extractor => ExtractorOps(o)
      case o: LaserTower => LaserTowerOps(o)
      case o: WarpLinker => WarpLinkerOps(o)
      case o: PopulationTower => PopulationTowerOps(o)
      case o: ActionTower => ActionTowerOps(o)

      /* Units */

      case o: Corvette => CorvetteOps(o)
      case o: Drone => DroneOps(o)
      case o: Fortress => FortressOps(o)
      case o: Gunship => GunshipOps(o)
      case o: WarpPrism => WarpPrismOps(o)
      case o: RayShip => RayShipOps(o)
      case o: RocketFrigate => RocketFrigateOps(o)
      case o: Scout => ScoutOps(o)
      case o: Wasp => WaspOps(o)
    }): WarpableOps[_]).asInstanceOf[WarpableOps[Self]]
}