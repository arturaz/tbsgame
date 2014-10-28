package app.models.game.world

import app.models.game.Attack
import app.models.game.events.{AttackedChangeEvt, AttackEvt, Evented}
import implicits._

trait FighterOps[Self <: Fighter] extends OwnedObjOps[Self]
with MoveAttackActionedOps[Self] {
  protected def attacked(value: Boolean)(self: Self): Self
  def attackedEvt(value: Boolean)(world: World, self: Self): Evented[Self] = {
    val newSelf = self |> attacked(value)
    Evented(
      newSelf,
      if (self === newSelf) Vector.empty
      else Vector(AttackedChangeEvt(world, newSelf))
    )
  }
}

trait FighterStats extends OwnedObjStats with MoveAttackActionedStats {
  val attack: Range.Inclusive
  val attackRange: TileDistance
}

trait FighterCompanion[Self <: Fighter] extends FighterOps[Self] with FighterStats

trait Fighter extends OwnedObj with MoveAttackActioned {
  type Self <: Fighter
  type Companion <: FighterOps[Self] with FighterStats

  val hasAttacked: Boolean

  override def teamTurnStartedSelf(world: World) =
    super.teamTurnStartedSelf(world) |> resetAttack

  protected def resetAttack(data: Evented[(World, Self)]) =
    data |>
    selfEventedUpdate(companion.attackedEvt(false)) |>
    selfEventedUpdate(companion.withMovedOrAttackedEvt(companion.InitialMovedOrAttacked))

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
  ): Either[String, (Attack, Evented[Self], Option[Target])] =
    cantAttackReason(obj, world).fold2({
      val attack = Attack(companion.attack.random, obj.defense.random)
      (
        attack,
        for {
          newSelf <- companion.attackedEvt(true)(world, self)
          newSelf <- companion.withMovedOrAttackedEvt(true)(world, newSelf)
        } yield newSelf,
        attack(obj)
      ).right
    }, _.left)

  def attack[Target <: OwnedObj](
    obj: Target, world: World
  ): Either[String, Evented[(World, Self, Attack, Option[Target])]] = {
    attackSimple(obj, world).right.map { case (attack, attackedEvt, newObj) =>
      for {
        attacked <- attackedEvt
        newWorld <-
          AttackEvt(world, attacked, obj -> newObj, attack) +:
          world.updated(self, attacked)
        newWorld <- newWorld.updated(obj, newObj)
      } yield (newWorld, attacked, attack, newObj)
    }
  }

  def attackWS(
    obj: OwnedObj, world: World
  ): Either[String, WObject.WorldObjUpdate[Self]] =
    attack(obj, world).right.map(_.map { t => (t._1, t._2) })

  def attackW(obj: OwnedObj, world: World): Either[String, Evented[World]] =
    attack(obj, world).right.map(_.map(_._1))
}