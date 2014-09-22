package app.models.world

import implicits._
import app.models.Attack

trait FighterStats extends OwnedObjStats with MoveAttackActionedStats {
  val attack: Range
  val attackRange: TileDistance
}

trait Fighter extends OwnedObj with MoveAttackActioned {
  type Self <: Fighter
  type Stats <: FighterStats

  val hasAttacked: Boolean

  override def teamTurnFinished =
    super.teamTurnFinished |> attacked(false) |> withMovedOrAttacked(false)

  def canAttack(obj: OwnedObj) =
    obj.bounds.withinTileDistance(position, stats.attackRange)

  def attack(obj: OwnedObj): Either[String, (Attack, Self)] =
    if (hasAttacked) Left(s"$self has already attacked!")
    else if (! canAttack(obj))
      s"$self cannot attack reach $obj - tile distance ${
        obj.bounds.tileDistance(position)
      } > attack range ${stats.attackRange}!".left
    else (
      Attack(stats.attack.random, obj.stats.defense.random),
      self |> attacked(true) |> withMovedOrAttacked(true)
    ).right

  protected def attacked(value: Boolean)(self: Self): Self
}