package app.models.world

import app.models.Attack
import app.models.game.events.{AttackEvt, Evented}
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

  override def teamTurnStartedSelf(world: World) =
    super.teamTurnStartedSelf(world) |> resetAttack

  protected def resetAttack(data: Evented[(World, Self)]) =
    data |>
    selfUpdate(_ |> companion.attacked(false)) |>
    selfUpdate(_ |> companion.withMovedOrAttacked(companion.InitialMovedOrAttacked))

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

  def attack(
    obj: OwnedObj, world: World
  ): Either[String, Evented[(World, Self, Attack)]] = {
    attack(obj).right.map { case (attack, attacked) =>
      Evented(
        (world.updated(this, attacked).update(attack, obj), attacked, attack),
        Vector(AttackEvt(id, obj.id, attack))
      )
    }
  }

  def attackWS(obj: OwnedObj, world: World): Either[String, Evented[(World, Self)]] =
    attack(obj, world).right.map(_.map { case (w, s, _) => (w, s) })

  def attackW(obj: OwnedObj, world: World): Either[String, Evented[World]] =
    attack(obj, world).right.map(_.map(_._1))
}