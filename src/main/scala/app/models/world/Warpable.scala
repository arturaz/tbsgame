package app.models.world

import implicits._

import app.models.Player

trait WarpableStats[W <: Warpable] extends OwnedObjStats {
  val InitialWarpState = 0
  val warpTime: Int
  val cost: Int

  def warp(world: World, owner: Player, position: Vect2): Either[String, W]
}

trait EmptySpaceWarpable[W <: Warpable] { _: WarpableStats[W] =>
  def warp(owner: Player, position: Vect2): W

  override def warp(
    world: World, owner: Player, position: Vect2
  ): Either[String, W] = {
    val b = bounds(position)
    if (world.isFree(b)) warp(owner, position).right
    else s"Can't warp in because $b is taken in $world".left
  }
}

trait EmptySpaceWarpableStats[W <: Warpable] extends WarpableStats[W]
with EmptySpaceWarpable[W]

trait Warpable extends OwnedObj {
  type Self <: Warpable
  type Stats <: WarpableStats[Self]

  val warpState: Int
  def isWarpingIn = warpState < stats.warpTime
  def isWarpedIn = warpState == stats.warpTime

  override def teamTurnFinished =
    if (isWarpedIn) super.teamTurnFinished
    else advanceWarpState(super.teamTurnFinished, warpState + 1)

  protected def advanceWarpState(self: Self, newState: Int): Self
}
