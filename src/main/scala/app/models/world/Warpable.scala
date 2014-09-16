package app.models.world

trait WarpableStats extends WObjectStats {
  val InitialWarpState = 0
  val warpTime: Int
  val cost: Int
}

trait Warpable extends WObject {
  type Stats <: WarpableStats

  val warpState: Int
  def isWarpingIn = warpState < stats.warpTime
  def isWarpedIn = warpState == stats.warpTime

  override def nextTurn =
    if (isWarpedIn) super.nextTurn
    else advanceWarpState(super.nextTurn, warpState + 1)

  protected def advanceWarpState(self: Self, newState: Int): Self
}
