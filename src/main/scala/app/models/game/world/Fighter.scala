package app.models.game.world

import app.models.game.events.{AttackEvt, AttacksChangedEvt, Evented, LevelChangeEvt}
import app.models.game.{Attack, Player}
import implicits._

import scala.language.implicitConversions

trait FighterStats extends OwnedObjStats {
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
  def InitialAttacks = attacks

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

trait FighterImpl extends OwnedObjImpl {
  val stats: FighterStats

  val attacksLeft: Attacks
  val xp: XP

  lazy val level = stats.level(xp)
  lazy val attackMultiplier = stats.LevelMultiplierTable(level)
  override def maxHp = stats.maxHpAt(level)
  def noAttacksLeft = attacksLeft.isZero
  def hasAttacksLeft = attacksLeft.isNotZero

  def canReachAttack(obj: OwnedObj) =
    obj.bounds.withinDistance(position, stats.attackRange)

  def cantAttackReason(obj: OwnedObj, world: World): Option[String] = {
    if (noAttacksLeft) Some(s"$this has already used all its attacks!")
    else if (isWarpingIn) Some(s"$this is still warping in!")
    else if (! world.isVisiblePartial(owner, obj.bounds))
      Some(s"$this cannot see $obj")
    else if (! canReachAttack(obj))
      Some(s"$this cannot attack reach $obj - tile distance ${
        obj.bounds.tileDistance(position)
      } > attack range ${stats.attackRange}!")
    else None
  }

  def canAttack(obj: OwnedObj, world: World) =
    cantAttackReason(obj, world).isEmpty

  def randomAttack: Atk =
    Atk((stats.randomAttack.value * attackMultiplier).round.toInt)
}

trait FighterOps[Self <: Fighter] {
  def self: Self

  def teamTurnFinished(world: World) =
    WObject.selfEventedUpdate(world, self, resetAttackIfWarpedIn(world))

  private[this] def resetAttackIfWarpedIn(world: World) =
    if (self.isWarpedIn) resetAttack(world) else Evented(self)

  private[this] def resetAttack(world: World): Evented[Self] =
    withAttacksLeftEvt(self.stats.attacks)(world)

  private[this] def attackSimple[Target <: OwnedObj](
    obj: Target, world: World
  ): Either[String, (Attack, Evented[Self], Option[Target])] =
    self.cantAttackReason(obj, world).fold2({
      val attack = Attack(self, obj)
      val newObj = attack(obj)
      (
        attack,
        for {
          newSelf <- self.withAttacksLeftEvt(
            self.attacksLeft - Attacks(1)
          )(world)
          newSelf <- newSelf.withNewXPEvt(
            newSelf.xp + XP(newObj.map(_ => 0).getOrElse(1))
          )(world)
          newSelf <- newSelf.withNewHPEvt(
            if (self.level == newSelf.level) newSelf.hp
            else (newSelf.hp + newSelf.maxHp - self.maxHp) min newSelf.maxHp
          )(world)
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
            val newOwner = respawnable.ownerAfterRespawn(self.owner)
            respawnable.respawn(world, newOwner)
          }
          ),
          // Not dead, just update
          _ => newWorld.updated(obj, newObjOpt)
        )
        newWorld <- self.owner.cast[Player].flatMap { p =>
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

  protected def withAttacksLeft(value: Attacks): Self

  def withAttacksLeftEvt(value: Attacks)(world: World): Evented[Self] = {
    val newSelf = withAttacksLeft(value)
    Evented(
      newSelf,
      if (self === newSelf) Vector.empty
      else Vector(AttacksChangedEvt(world, newSelf))
    )
  }

  protected def withNewXP(value: XP): Self

  def withNewXPEvt(value: XP)(world: World): Evented[Self] = {
    val newSelf = withNewXP(value)
    Evented(
      newSelf,
      if (self.level == newSelf.level) Vector.empty
      else Vector(LevelChangeEvt(world, newSelf))
    )
  }
}

trait ToFighterOps {
  implicit def toFighterOps[A <: Fighter](a: A): FighterOps[A] = (a match {

  }).asInstanceOf[FighterOps[A]]
}

object Fighter extends ToFighterOps