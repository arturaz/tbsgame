package app.models.game.world

import akka.event.LoggingAdapter
import app.models.game.{Player, Attack}
import app.models.game.events.{LevelChangeEvt, AttacksChangedEvt, AttackEvt, Evented}
import implicits._

import scala.util.Random

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

  protected def withNewXP(value: XP)(self: Self): Self
  def withNewXPEvt(value: XP)(world: World, self: Self): Evented[Self] = {
    val newSelf = self |> withNewXP(value)
    Evented(
      newSelf,
      if (self.level == newSelf.level) Vector.empty
      else Vector(LevelChangeEvt(world, newSelf))
    )
  }

  def randomAttack(f: Fighter): Atk =
    Atk((f.companion.randomAttack.value * f.attackMultiplier).round.toInt)
}

trait FighterStats extends OwnedObjStats with MoveAttackActionedStats {
  val attack: Atk
  val attackSpread = AtkSpread(0.3)
  // lazy because attackSpread might be overriden
  lazy val attackDamageRange = {
    val from = Atk((attack.value * (1 - attackSpread.value)).round.toInt)
    val to = Atk((attack.value * (1 + attackSpread.value)).round.toInt)
    AtkRange(from, to)
  }
  def randomAttack: Atk = Atk(attackDamageRange.random)

  val attackRange: RadialDistance
  val attacks: Attacks
  val critical: Chance = Chance(0.1)
  val criticalMultiplier: Double = 2
  @inline def InitialAttacks = attacks

  val LevelMultiplierTable = Map(
    Level(1) -> 1.15, Level(2) -> 1.45, Level(3) -> 2.0
  ).withDefaultValue(1.0)
  def maxHpAt(l: Level) = HP((maxHp.value * LevelMultiplierTable(l)).round.toInt)

  val InitialXP = XP(0)
  /* XP needed -> Level reached */
  val XPTable = IndexedSeq(
    XP(3) -> Level(1),
    XP(8) -> Level(2),
    XP(20) -> Level(3)
  )

  def level(xp: XP) = XPTable.collectFirst {
    case (lvlXP, level) if xp < lvlXP => level - Level(1)
  }.getOrElse(XPTable.last._2)
}

trait FighterCompanion[Self <: Fighter] extends FighterOps[Self] with FighterStats

trait Fighter extends OwnedObj with MoveAttackActioned { traitSelf =>
  type Self >: traitSelf.type  <: Fighter
  type Companion <: FighterOps[Self] with FighterStats

  val attacksLeft: Attacks
  lazy val level = companion.level(xp)
  lazy val attackMultiplier = companion.LevelMultiplierTable(level)
  val xp: XP
  def maxHp = companion.maxHpAt(level)

  override def teamTurnStartedSelf(world: World)(implicit log: LoggingAdapter) =
    super.teamTurnStartedSelf(world) |> resetAttack

  protected def resetAttack(data: Evented[(World, Self)]) =
    data |>
    selfEventedUpdate(companion.withAttacksLeftEvt(companion.attacks)) |>
    selfEventedUpdate(companion.withMovedOrAttackedEvt(companion.InitialMovedOrAttacked))

  def noAttacksLeft = attacksLeft.isZero
  def hasAttacksLeft = attacksLeft.isNotZero

  def canReachAttack(obj: OwnedObj) =
    obj.bounds.withinDistance(position, companion.attackRange)

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
      val newObj = attack(obj)
      (
        attack,
        for {
          newSelf <- companion.withAttacksLeftEvt(
            self.attacksLeft - Attacks(1)
          )(world, self)
          newSelf <- companion.withMovedOrAttackedEvt(true)(world, newSelf)
          newSelf <- companion.withNewXPEvt(
            newSelf.xp + XP(newObj.map(_ => 0).getOrElse(1))
          )(world, newSelf)
          newSelf <- companion.withNewHPEvt(
            if (self.level == newSelf.level) newSelf.hp
            else newSelf.hp max newSelf.maxHp
          )(world, newSelf)
        } yield newSelf,
        newObj
      ).right
    }, _.left)

  def attack[Target <: OwnedObj](
    obj: Target, world: World
  ): Either[String, Evented[(World, Self, Attack, Option[Target])]] = {
    attackSimple(obj, world).right.map { case (attack, attackedEvt, newObjOpt) =>
      for {
        attacked <- attackedEvt
        newWorld <-
          AttackEvt(world, attacked, obj -> newObjOpt, attack) +:
          world.updated(self, attacked)
        newWorld <- newObjOpt.fold2(
          // New object is dead, need to respawn
          obj.cast[RespawnsOnDestruction].fold2(
            // Not respawnable, just remove
            newWorld.updated(obj, newObjOpt),
            // Respawn
            { respawnable =>
              val newOwner = respawnable.ownerAfterRespawn(owner)
              respawnable.respawn(world, newOwner)
            }
          ),
          // Not dead, just update
          _ => newWorld.updated(obj, newObjOpt)
        )
        newWorld <- owner.cast[Player].flatMap { p =>
          newObjOpt.fold2(obj.destroyReward.map((p, _)), _ => None)
        }.fold2(Evented(newWorld), { case (player, resources) =>
          newWorld.addResources(player, resources).right.get
        })
      } yield (newWorld, attacked, attack, newObjOpt)
    }
  }

  def attackWS(
    obj: OwnedObj, world: World
  ): Either[String, WObject.WorldObjUpdate[Self]] =
    attack(obj, world).right.map(_.map { t => (t._1, t._2) })

  def attackW(obj: OwnedObj, world: World): Either[String, Evented[World]] =
    attack(obj, world).right.map(_.map(_._1))
}