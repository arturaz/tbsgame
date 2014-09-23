package app.models.world

import implicits._

import app.models.Player

trait WarpableOps[Self <: Warpable] extends OwnedObjOps[Self] {
  def warp(world: World, owner: Player, position: Vect2): Either[String, Self]
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

  override def warp(
    world: World, owner: Player, position: Vect2
    ): Either[String, Self] = {
    val b = bounds(position)
    if (world.isFree(b)) warp(owner, position).right
    else s"Can't warp in because $b is taken in $world".left
  }
}

trait EmptySpaceWarpableCompanion[Self <: Warpable]
extends WarpableCompanion[Self] with EmptySpaceWarpableOps[Self]