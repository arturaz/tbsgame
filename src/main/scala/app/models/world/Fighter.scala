package app.models.world

import implicits._
import app.models.Attack

/**
 * Created by arturas on 2014-09-11.
 */
trait FighterStats extends FactionObjStats {
  val attack: Range
  val attackRange: TileRange
}

object Fighter {
  def canAttack(pos: Vect2, attackRange: TileRange, targetBounds: Bounds) =
    targetBounds.perimeter.exists(pos.tileDistance(_) <= attackRange.range)
}

trait Fighter extends FactionObj {
  type Self <: Fighter
  type Stats <: FighterStats

  val hasAttacked: Boolean

  override def nextTurn = attacked(super.nextTurn, false)

  def canAttack(obj: FactionObj) =
    Fighter.canAttack(position, stats.attackRange, obj.bounds)

  def attack(obj: FactionObj): Either[String, (Attack, Self)] =
    if (hasAttacked) Left(s"$self has already attacked!")
    else if (! canAttack(obj)) Left(s"$self cannot attack reach $obj!")
    else Right((
      Attack(stats.attack.random, obj.stats.defense.random),
      attacked(self, true)
    ))
  protected def attacked(self: Self, value: Boolean): Self
}