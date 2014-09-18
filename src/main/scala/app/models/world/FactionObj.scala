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
  type Stats <: FactionObjStats
  val hp: Int
  val owner: Owner
  def isEnemy(o: FactionObj) = owner.team != o.owner.team
  def isFriend(o: FactionObj) = ! isEnemy(o)

  lazy val visibility = {
    val vis = stats.visibility
    Bounds(
      bounds.x.start - vis to bounds.x.end + vis,
      bounds.y.start - vis to bounds.y.end + vis
    )
  }
  def sees(obj: WObject) = visibility.intersects(obj.bounds)

  def takeDamage: Option[Self] =
    if (hp == 1) None else Some(withNewHp(hp - 1))
  protected def withNewHp(hp: Int): Self
}
