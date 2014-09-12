package app.models.world

import app.models.Owner

trait FactionObjStats extends WObjectStats {
  val maxHp: Int
  val visibility: Int
  val defense: Range

  protected def emptyRange = 0 until 0
}

/* Object that belongs to some faction and not just a world prop */
trait FactionObj extends WObject {
  def stats: FactionObjStats
  var hp = stats.maxHp
  var owner: Owner

  lazy val visibility = Bounds(
    bounds.x.start - stats.visibility to bounds.x.end + stats.visibility,
    bounds.y.start - stats.visibility to bounds.y.end + stats.visibility
  )
}
