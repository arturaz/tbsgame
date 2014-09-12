package app.models.world

/**
 * Created by arturas on 2014-09-11.
 */
trait FighterStats extends FactionObjStats {
  val attack: Range
  val attackRange: Int
}

trait Fighter extends FactionObj {
  def stats: FighterStats
}