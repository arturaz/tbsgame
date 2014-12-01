package app.models.game.world

import akka.event.LoggingAdapter
import app.models.game.events.{WarpStateChangeEvt, WarpEvt, Evented}
import implicits._

import app.models.game.{Actions, Player}

import scala.util.Random

trait WarpableOps[Self <: Warpable] extends OwnedObjOps[Self] {
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

  def setWarpState(newState: WarpTime)(self: Self): Self
  def nextWarpState(world: World, self: Self): Evented[Self] =
    if (self.isWarpedIn) Evented(self)
    else {
      val newSelf = self |> setWarpState(self.warpState + WarpTime(1))
      Evented(newSelf, Vector(WarpStateChangeEvt(world, newSelf)))
    }
}

trait WarpableStats extends OwnedObjStats {
  val InitialWarpState = WarpTime(0)
  val warpTime: WarpTime
  val cost: Resources
  val group: WarpableGroup
}

trait WarpableCompanion[Self <: Warpable] extends WarpableOps[Self]
with WarpableStats

object WarpableCompanion {
  type Some = WarpableCompanion[_ <: Warpable]
}

object Warpable {
  /* Actions needed to warp in something */
  val ActionsNeeded = Actions(1)
}

trait Warpable extends OwnedObj {
  type Self <: Warpable
  type Companion <: WarpableOps[Self] with WarpableStats

  val warpState: WarpTime
  override def isWarpingIn = warpState < companion.warpTime
  override def destroyReward = Some(Resources(
    (companion.cost.value * Random.double(0.1, 1f / 3)).round.toInt
  ))

  override def teamTurnStartedSelf(world: World)(implicit log: LoggingAdapter) =
    super.teamTurnStartedSelf(world) |>
    selfEventedUpdate(companion.nextWarpState)
}

trait EmptySpaceWarpableOps[Self <: Warpable] { _: WarpableOps[Self] =>
  def warp(owner: Player, position: Vect2): Self

  override def warpWOReactionImpl(
    world: World, owner: Player, position: Vect2
  ): Either[String, Self] = {
    val b = bounds(position)
    if (! world.canWarp(b)) s"Can't warp in because $b is taken in $world".left
    else warp(owner, position).right
  }
}

trait EmptySpaceWarpableCompanion[Self <: Warpable]
extends WarpableCompanion[Self] with EmptySpaceWarpableOps[Self]

sealed trait WarpableGroup
object WarpableGroup {
  case object Building extends WarpableGroup
  case object Unit extends WarpableGroup
}