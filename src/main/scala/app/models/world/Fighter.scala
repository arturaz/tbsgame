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

trait Fighter extends FactionObj {
  type Self <: Fighter
  type Stats <: FighterStats

  val hasAttacked: Boolean

  def attack(obj: FactionObj): Either[String, (Attack, Self)] =
    if (hasAttacked)
      Left(s"$self has already attacked!")
    else if (position.tileDistance(obj.position) > stats.attackRange.range)
      Left(s"$self cannot reach $obj!")
    else
      Right((
        Attack(stats.attack.random, obj.stats.defense.random),
        attacked
      ))
  protected def attacked: Self
}