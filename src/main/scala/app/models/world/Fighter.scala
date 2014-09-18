package app.models.world

import implicits._
import app.models.Attack

/**
 * Created by arturas on 2014-09-11.
 */
trait FighterStats extends FactionObjStats {
  val attack: Range
  val attackRange: TileDistance
}

trait Fighter extends FactionObj {
  type Self <: Fighter
  type Stats <: FighterStats

  val hasAttacked: Boolean

  override def nextTurn = attacked(super.nextTurn, false)

  def canAttack(obj: FactionObj) =
    obj.bounds.withinTileDistance(position, stats.attackRange)

  def attack(obj: FactionObj): Either[String, (Attack, Self)] =
    if (hasAttacked) Left(s"$self has already attacked!")
    else if (! canAttack(obj)) {
      println(obj)
      println(obj.bounds)
      println(obj.bounds.points.toList)
      println(obj.bounds.corners.toList)
      println(obj.bounds.perimeter.toList)
      println(obj.bounds.center)
      Left(s"$self cannot attack reach $obj - tile distance ${
        obj.bounds.tileDistance(position)
      } > attack range ${stats.attackRange}!")
    }
    else Right((
      Attack(stats.attack.random, obj.stats.defense.random),
      attacked(self, true)
    ))
  protected def attacked(self: Self, value: Boolean): Self
}