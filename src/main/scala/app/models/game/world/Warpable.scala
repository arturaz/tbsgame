package app.models.game.world

import akka.event.LoggingAdapter
import app.models.game.events.{Evented, WarpEvt, WarpStateChangeEvt}
import app.models.game.world.buildings._
import app.models.game.world.units._
import app.models.game.{Actions, Player, Population}
import implicits._

import scala.language.implicitConversions

sealed trait WarpableGroup
object WarpableGroup {
  case object Building extends WarpableGroup
  case object Unit extends WarpableGroup
}

trait WarpableCompanion[Self <: Warpable] { _: WObjectStats =>
  /* Create Warpable at given position. */
  protected def warpWOReactionImpl(
    world: World, owner: Player, position: Vect2
  ): Either[String, Self]

  private[this] def warpWOReaction(
    world: World, owner: Player, position: Vect2
  ): Either[String, Self] = {
    val b = bounds(position)
    if (world.isVisibleFull(owner, b)) warpWOReactionImpl(world, owner, position)
    else s"$b is not fully visible for $owner".left
  }

  /* Warp with reactions applied. */
  def warp(
    world: World, player: Player, position: Vect2
  ): Either[String, WObject.WorldObjOptUpdate[Self]] =
    warpWOReaction(world, player, position).right.map { warpedIn =>
      World.revealObjects(
        player.team, WarpEvt(world, warpedIn) +: world.add(warpedIn)
      ).flatMap(_.reactTo(warpedIn))
    }

  def warpW(
    world: World, player: Player, position: Vect2
  ): Either[String, Evented[World]] =
    warp(world, player, position).right.map { _.map(_._1) }
}

trait EmptySpaceWarpableCompanion[Self <: Warpable] extends WarpableCompanion[Self] {
_: WObjectStats =>
  def warp(owner: Player, position: Vect2): Self

  override def warpWOReactionImpl(
    world: World, owner: Player, position: Vect2
  ): Either[String, Self] = {
    val b = bounds(position)
    if (! world.canWarp(b)) s"Can't warp in because $b is taken in $world".left
    else warp(owner, position).right
  }
}

object WarpableCompanion {
  type Some = WarpableStats with WarpableCompanion[_ <: Warpable]
}

trait WarpableStats extends OwnedObjStats {
  val InitialWarpState = WarpTime(0)
  val warpTime: WarpTime
  val cost: Resources
  val populationCost: Population
  val group: WarpableGroup
}

trait WarpableImpl extends OwnedObjImpl {
  val warpState: WarpTime
  type Stats <: WarpableStats

  override def isWarpingIn = warpState < stats.warpTime
//  override def destroyReward = Some(Resources(
//    (companion.cost.value * Random.double(0.1, 1f / 3)).round.toInt
//  ))
}

trait WarpableOps[Self <: Warpable] extends OwnedObjOps[Self] {
  def setWarpState(newState: WarpTime): Self
  def nextWarpState(world: World): Evented[Self] =
    if (self.isWarpedIn) Evented(self)
    else {
      val newSelf = setWarpState(self.warpState + WarpTime(1))
      Evented(newSelf, Vector(WarpStateChangeEvt(world, newSelf)))
    }

  def teamTurnStarted(world: World)(implicit log: LoggingAdapter) =
    WObject.selfEventedUpdate(world, self, nextWarpState(world))
}

trait ToWarpableOps {
  implicit def toWarpableOps[Self <: Warpable](a: Self): WarpableOps[Self] = (a match {
    /* Buildings */

    case o: Extractor => ExtractorOps(o)
    case o: LaserTower => LaserTowerOps(o)
    case o: WarpLinker => WarpLinkerOps(o)

    /* Units */

    case o: Corvette => CorvetteOps(o)
    case o: Fortress => FortressOps(o)
    case o: Gunship => GunshipOps(o)
    case o: RayShip => RayShipOps(o)
    case o: RocketFrigate => RocketFrigateOps(o)
    case o: Scout => ScoutOps(o)
    case o: Wasp => WaspOps(o)
  }).asInstanceOf[WarpableOps[Self]]
}