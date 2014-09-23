package app.models.world

import app.models.Attack
import implicits._

trait FighterOps[Self] extends OwnedObjOps[Self]
with MoveAttackActionedOps[Self] {
  def attacked(value: Boolean)(self: Self): Self
}

trait FighterStats extends OwnedObjStats with MoveAttackActionedStats {
  val attack: Range
  val attackRange: TileDistance
}

trait FighterCompanion[Self] extends FighterOps[Self] with FighterStats

trait Fighter extends OwnedObj with MoveAttackActioned {
  type Self <: Fighter
  type Companion <: FighterOps[Self] with FighterStats

  val hasAttacked: Boolean

  override def teamTurnFinishedSelf(world: World) =
    super.teamTurnFinishedSelf(world) |>
    selfUpdate(_ |> companion.attacked(false)) |>
    selfUpdate(_ |> companion.withMovedOrAttacked(false))

  def canAttack(obj: OwnedObj) =
    obj.bounds.withinTileDistance(position, companion.attackRange)

  def attack(obj: OwnedObj): Either[String, (Attack, Self)] =
    if (hasAttacked) Left(s"$self has already attacked!")
    else if (! canAttack(obj))
      s"$self cannot attack reach $obj - tile distance ${
        obj.bounds.tileDistance(position)
      } > attack range ${companion.attackRange}!".left
    else (
      Attack(companion.attack.random, obj.companion.defense.random),
      self |> companion.attacked(true) |> companion.withMovedOrAttacked(true)
    ).right
}