package app.models.world

import app.models.{Player, Owner}

trait OwnedObjStats extends WObjectStats {
  val maxHp: Int
  val visibility: Int
  val defense: Range
  /* If team has no critical objects it cannot do any more actions and
     loses the game. */
  val isCritical: Boolean = false

  protected def emptyRange = 0 until 0
}

/* Object that belongs to some faction and not just a world prop */
trait OwnedObj extends WObject {
  type Stats <: OwnedObjStats
  val hp: Int
  val owner: Owner
  def isEnemy(o: OwnedObj) = owner.team != o.owner.team
  def isFriend(o: OwnedObj) = ! isEnemy(o)

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

  /* Called when team turn is finished to get a new version of self. */
  def teamTurnFinished = self
}

trait PlayerObj extends OwnedObj {
  val owner: Player
}
