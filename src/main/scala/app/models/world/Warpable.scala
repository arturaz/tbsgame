package app.models.world

trait WarpableStats {
  val warpTime: Int
  val cost: Int
}

trait Warpable extends WObject {
  def stats: WarpableStats

  private[this] var turnsSinceWarpStart = 0
  def isWarpedIn = turnsSinceWarpStart == stats.warpTime

  override def nextTurn() = {
    super.nextTurn()
    if (turnsSinceWarpStart < stats.warpTime) turnsSinceWarpStart += 1
  }
}
