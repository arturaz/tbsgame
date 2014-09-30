package app.models.world

import app.models.game.events.{WarpEvt, Evented}
import implicits._

import app.models.Player

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
      Evented(world.add(warpedIn), Vector(WarpEvt(warpedIn))).
        flatMap(_.reactTo(warpedIn))
    }

  def warpW(
    world: World, player: Player, position: Vect2
  ): Either[String, Evented[World]] =
    warp(world, player, position).right.map { _.map(_._1) }

  def setWarpState(newState: Int)(self: Self): Self
  def nextWarpState(self: Self) =
    if (self.isWarpedIn) self
    else self |> setWarpState(self.warpState + 1)
}

trait WarpableStats extends OwnedObjStats {
  val InitialWarpState = 0
  val warpTime: Int
  val cost: Int
}

trait WarpableCompanion[Self <: Warpable] extends WarpableOps[Self]
with WarpableStats

trait Warpable extends OwnedObj {
  type Self <: Warpable
  type Companion <: WarpableOps[Self] with WarpableStats

  val warpState: Int
  def isWarpingIn = warpState < companion.warpTime
  def isWarpedIn = warpState == companion.warpTime

  override def teamTurnStartedSelf(world: World) =
    super.teamTurnStartedSelf(world) |>
    selfUpdate(companion.nextWarpState)
}

trait EmptySpaceWarpableOps[Self <: Warpable] { _: WarpableOps[Self] =>
  def warp(owner: Player, position: Vect2): Self

  override def warpWOReactionImpl(
    world: World, owner: Player, position: Vect2
  ): Either[String, Self] = {
    val b = bounds(position)
    if (! world.isFree(b)) s"Can't warp in because $b is taken in $world".left
    else warp(owner, position).right
  }
}

trait EmptySpaceWarpableCompanion[Self <: Warpable]
extends WarpableCompanion[Self] with EmptySpaceWarpableOps[Self]