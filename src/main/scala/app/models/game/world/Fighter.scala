package app.models.game.world

import app.models.game.Attack
import app.models.game.events.{AttackEvt, Evented}
import implicits._

trait FighterOps[Self] extends OwnedObjOps[Self]
with MoveAttackActionedOps[Self] {
  def attacked(value: Boolean)(self: Self): Self
}

trait FighterStats extends OwnedObjStats with MoveAttackActionedStats {
  val attack: Range.Inclusive
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

  def canReachAttack(obj: OwnedObj) =
    obj.bounds.withinTileDistance(position, companion.attackRange)

  def cantAttackReason(obj: OwnedObj, world: World): Option[String] = {
    if (hasAttacked) Some(s"$self has already attacked!")
    else if (! world.isVisiblePartial(owner, obj.bounds))
      Some(s"$self cannot see $obj")
    else if (! canReachAttack(obj))
      Some(s"$self cannot attack reach $obj - tile distance ${
        obj.bounds.tileDistance(position)
      } > attack range ${companion.attackRange}!")
    else None
  }

  def canAttack(obj: OwnedObj, world: World) = cantAttackReason(obj, world).isEmpty

  private[this] def attackSimple[Target <: OwnedObj](
    obj: Target, world: World
  ): Either[String, (Attack, Self, Option[Target])] =
    cantAttackReason(obj, world).fold2({
      val attack = Attack(companion.attack.random, obj.companion.defense.random)
      (
        attack,
        self |> companion.attacked(true) |> companion.withMovedOrAttacked(true),
        attack(obj)
      ).right
    }, _.left)

  def attack[Target <: OwnedObj](
    obj: Target, world: World
  ): Either[String, Evented[(World, Self, Attack, Option[Target])]] = {
    attackSimple(obj, world).right.map { case (attack, attacked, newObj) =>
      (
        AttackEvt(world, attacked, obj -> newObj, attack) +:
        world.updated(this, attacked)
      ).flatMap(_.updated(obj, newObj)).map((_, attacked, attack, newObj))
    }
  }

  def attackWS(
    obj: OwnedObj, world: World
  ): Either[String, WObject.WorldObjUpdate[Self]] =
    attack(obj, world).right.map(_.map { t => (t._1, t._2) })

  def attackW(obj: OwnedObj, world: World): Either[String, Evented[World]] =
    attack(obj, world).right.map(_.map(_._1))
}