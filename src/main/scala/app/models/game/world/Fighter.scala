package app.models.game.world

import app.models.game.{Player, Attack}
import app.models.game.events.{AttacksChangedEvt, AttackEvt, Evented}
import implicits._

trait FighterOps[Self <: Fighter] extends OwnedObjOps[Self]
with MoveAttackActionedOps[Self] {
  protected def withAttacksLeft(value: Attacks)(self: Self): Self
  def withAttacksLeftEvt(value: Attacks)(world: World, self: Self): Evented[Self] = {
    val newSelf = self |> withAttacksLeft(value)
    Evented(
      newSelf,
      if (self === newSelf) Vector.empty
      else Vector(AttacksChangedEvt(world, newSelf))
    )
  }
}

trait FighterStats extends OwnedObjStats with MoveAttackActionedStats {
  val attack: Range.Inclusive
  val attackRange: TileDistance
  val attacks: Attacks
  val critical: Chance = Chance(0.1)
  val criticalMultiplier: Double = 2
  @inline def InitialAttacks = attacks
}

trait FighterCompanion[Self <: Fighter] extends FighterOps[Self] with FighterStats

trait Fighter extends OwnedObj with MoveAttackActioned { traitSelf =>
  type Self >: traitSelf.type  <: Fighter
  type Companion <: FighterOps[Self] with FighterStats

  val attacksLeft: Attacks

  override def teamTurnStartedSelf(world: World) =
    super.teamTurnStartedSelf(world) |> resetAttack

  protected def resetAttack(data: Evented[(World, Self)]) =
    data |>
    selfEventedUpdate(companion.withAttacksLeftEvt(companion.attacks)) |>
    selfEventedUpdate(companion.withMovedOrAttackedEvt(companion.InitialMovedOrAttacked))

  def noAttacksLeft = attacksLeft.isZero
  def hasAttacksLeft = attacksLeft.isNotZero

  def canReachAttack(obj: OwnedObj) =
    obj.bounds.withinTileDistance(position, companion.attackRange)

  def cantAttackReason(obj: OwnedObj, world: World): Option[String] = {
    if (noAttacksLeft) Some(s"$self has already used all its attacks!")
    else if (isWarpingIn) Some(s"$self is still warping in!")
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
      val attack = Attack(this, obj)
      (
        attack,
        for {
          newSelf <- companion.withAttacksLeftEvt(
            self.attacksLeft - Attacks(1)
          )(world, self)
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
        newWorld <- owner.cast[Player].flatMap { p =>
          newObj.fold2(obj.destroyReward.map((p, _)), _ => None)
        }.fold2(Evented(newWorld), { case (player, resources) =>
          newWorld.addResources(player, resources).right.get
        })
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