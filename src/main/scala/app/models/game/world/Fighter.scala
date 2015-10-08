package app.models.game.world

import akka.event.LoggingAdapter
import app.models.game.events._
import app.models.game.world.buildings.LaserTowerOps
import app.models.game.world.units._
import app.models.game.{Attack, Player}
import implicits._
import app.models.game.world.Ops._

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scalaz._, Scalaz._

trait FighterStatsImpl { _: FighterStats =>
  val attack: Atk
  val attackOverrides = Map.empty[WObjKind, Atk]
  val attackSpread = AtkSpread(0.15)

  def attackTo(kind: WObjKind) = attackOverrides.getOrElse(kind, attack)

  def attackDamageRange(base: Atk) = {
    val from = Atk((base.value * (1 - attackSpread.value)).round.toInt)
    val to = Atk((base.value * (1 + attackSpread.value)).round.toInt)
    AtkRange(from, to)
  }

  def attackDamageRangeTo(kind: WObjKind) = attackDamageRange(attackTo(kind))
  def randomAttackTo(kind: WObjKind) = Atk(attackDamageRangeTo(kind).random)

  val attackRange: RadialDistance
  val attacks: Attacks
  val critical = Chance(0.05)
  val criticalMultiplier = 2d
  val InitialAttacks = Attacks(0)
  val retaliates = false

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
  type Stats <: FighterStats

  val attacksLeft: Attacks
  val xp: XP

  lazy val level = stats.level(xp)
  lazy val attackMultiplier = stats.LevelMultiplierTable(level)
  override def maxHp = stats.maxHpAt(level)
  def noAttacksLeft = attacksLeft.isZero
  def hasAttacksLeft = attacksLeft.isNotZero

  def canReachAttack(bounds: Bounds) =
    bounds.withinDistance(position, stats.attackRange)

  def cantAttackReason(
    bounds: Bounds, world: World, checkVisibility: Boolean
  ): Option[String] = {
    if (noAttacksLeft) Some(s"$this has already used all its attacks!")
    else if (isWarpingIn) Some(s"$this is still warping in!")
    else if (checkVisibility && ! world.isVisiblePartial(owner, bounds))
      Some(s"$this cannot see $bounds")
    else if (! canReachAttack(bounds))
      Some(s"$this cannot attack reach $bounds - tile distance ${
        bounds.tileDistance(position)
      } > attack range ${stats.attackRange}!")
    else None
  }

  def cantAttackReason(
    obj: OwnedObj, world: World, checkVisibility: Boolean
  ): Option[String] = {
    cantAttackReason(obj.bounds, world, checkVisibility).map { err =>
      s"Can't attack $obj: $err"
    }
  }

  def canAttack(obj: OwnedObj, world: World, checkVisibility: Boolean=true) =
    cantAttackReason(obj, world, checkVisibility).isEmpty

  def randomAttackTo(kind: WObjKind): Atk =
    Atk((stats.randomAttackTo(kind).value * attackMultiplier).round.toInt)
}

trait FighterOps[Self <: Fighter] {
  def self: Self

  final def fighterTeamTurnStarted(world: World)(implicit log: LoggingAdapter) =
    WObject.selfEventedUpdate(world, self, resetAttackIfWarpedIn(world))

  private[this] def resetAttackIfWarpedIn(world: World)(implicit log: LoggingAdapter) =
    if (self.isWarpedIn) resetAttack(world)
    else {
      log.debug("not resetting attack because not warped in {}", self)
      Evented(self)
    }

  private[this] def resetAttack(world: World)(implicit log: LoggingAdapter)
  : Evented[Self] =
    withAttacksLeftEvt(self.stats.attacks)(world)

  private[this] def attackSimple[Target <: OwnedObj]
  (obj: Target, world: World, checkVisibility: Boolean)(implicit log: LoggingAdapter)
  : String \/ (Attack, Evented[Self], Option[Target]) =
    self.cantAttackReason(obj, world, checkVisibility).fold2({
      val attack = Attack(self, obj)
      val newObj = attack(obj)
      (
        attack,
        for {
          newSelf <- withAttacksLeftEvt(
            self.attacksLeft - Attacks(1)
          )(world)
          newSelf <- newSelf.withNewXPEvt(
            newSelf.xp + XP(newObj.fold2(1, _ => 0))
          )(world)
          newSelf <- newSelf.withNewHPEvt(
            if (self.level == newSelf.level) newSelf.hp
            else (newSelf.hp + newSelf.maxHp - self.maxHp) min newSelf.maxHp
          )(world)
        } yield newSelf,
        newObj
      ).right
    }, _.left)



  def attackPosW(pos: Vect2, world: World)(implicit log: LoggingAdapter)
  : String \/ Evented[World] = attackPos(pos, world).map(_.map(_._1))

  def attackPos(pos: Vect2, world: World)(implicit log: LoggingAdapter)
  : String \/ Evented[(World, Option[Self])] = {
    val targetOpt = world.objects.objectsIn(pos).collectFirst {
      case obj: OwnedObj if self.isEnemy(obj) => obj
    }
    targetOpt match {
      case None =>
        self.cantAttackReason(pos.toBounds, world, checkVisibility = false) match {
          case None =>
            val evented = (
              AttackPosEvt(
                world.visibilityMap, self, pos
              ) +: withAttacksLeftEvt(self.attacksLeft - Attacks(1))(world)
            ).flatMap { newSelf =>
              world.updated(self, newSelf).map { world => (world, Some(newSelf)) }
            }
            evented.right
          case Some(err) =>
            err.left
        }
      case Some(target) =>
        attack(target, world, checkVisibility = false).map { evented =>
          val t = evented.value
          val events = evented.events.map {
            case e: AttackEvt[_] => AttackPosEvt(e.visibilityMap, e.attacker, pos)
            case e => e
          }
          Evented((t._1, t._2), events)
        }
    }
  }

  def attack[Target <: OwnedObj](
    obj: Target, world: World,
    invokeRetaliation: Boolean=true, checkVisibility: Boolean=true
  )(implicit log: LoggingAdapter)
  : String \/ Evented[(World, Option[Self], Attack, Option[Target])] = {
    val origAtkEvtE = attackSimple(obj, world, checkVisibility).map {
      case (attack, attackedEvt, newObjOpt) =>
        for {
          attacked <- attackedEvt
          newWorld <-
            AttackEvt(world.visibilityMap, attacked, obj -> newObjOpt, attack) +:
              world.updated(self, attacked)
          newWorld <- newObjOpt.fold2(
            // New object is dead, need to respawn
            obj.cast[RespawnsOnDestruction].fold2(
              // Not respawnable, just remove
              newWorld.updated(obj, newObjOpt),
              // Respawn
              { respawnable =>
                val newOwner = respawnable.ownerAfterRespawn(self.owner)
                respawnable.respawn(newWorld, newOwner)
              }
            ),
            // Not dead, just update
            _ => newWorld.updated(obj, newObjOpt)
          )
          newWorld <- self.owner.cast[Player].flatMap { p =>
            newObjOpt.fold2(obj.destroyReward.map((p, _)), _ => None)
          }.fold2(Evented(newWorld), { case (player, resources) =>
            newWorld.addResources(player, resources).right_!
          })
        } yield (newWorld, attacked, attack, newObjOpt)
    }

    def toRet(t: (World, Self, Attack, Option[Target])) = (t._1, Some(t._2), t._3, t._4)
    def toRetE(t: (World, Self, Attack, Option[Target])) = Evented(toRet(t))
    // Do retaliation if possible.
    origAtkEvtE.map { _.flatMap {
      case orig @ (world, self, attack, Some(targetFighter: Fighter))
      if targetFighter.stats.retaliates && invokeRetaliation &&
      targetFighter.canAttack(self, world) =>
        val target = targetFighter.asInstanceOf[Target with Fighter]
        target.attack(self, world, invokeRetaliation = false).fold(
          err => {
            log.error("Error while retaliating with {} to {}: {}", target, self, err)
            toRetE(orig)
          },
          evtRetaliation => evtRetaliation.map { case (world, targetOpt, atk, selfOpt) =>
            (world, selfOpt, atk, targetOpt)
          }
        )
      case orig => toRetE(orig)
    } }
  }

  def attackWS(obj: OwnedObj, world: World)(implicit log: LoggingAdapter)
  : String \/ WObject.WorldObjUpdate[Option[Self]] =
    attack(obj, world).map(_.map { t => (t._1, t._2) })

  def attackW(obj: OwnedObj, world: World)(implicit log: LoggingAdapter)
  : String \/ Evented[World] =
    attack(obj, world).map(_.map(_._1))

  protected def withAttacksLeft(value: Attacks): Self

  def withAttacksLeftEvt(value: Attacks)(world: World)(implicit log: LoggingAdapter)
  : Evented[Self] = {
    val newSelf = withAttacksLeft(value)
    Evented(
      newSelf,
      if (self === newSelf) {
        log.debug("not creating attacks changed evt, because {} === {}", self, newSelf)
        Vector.empty
      }
      else Vector(AttacksChangedEvt(world.visibilityMap, newSelf))
    )
  }

  protected def withNewXP(value: XP): Self

  def withNewXPEvt(value: XP)(world: World): Evented[Self] = {
    val newSelf = withNewXP(value)
    Evented(
      newSelf,
      if (self.level == newSelf.level) Vector.empty
      else Vector(LevelChangeEvt(world.visibilityMap, newSelf))
    )
  }
}

trait ToFighterOps {
  implicit def toFighterOps[A <: Fighter](a: A): FighterOps[A] = ((a match {
    /* Buildings */

    case o: LaserTower => LaserTowerOps(o)

    /* Units */

    case o: Corvette => CorvetteOps(o)
    case o: Gunship => GunshipOps(o)
    case o: RocketFrigate => RocketFrigateOps(o)
    case o: RayShip => RayShipOps(o)
    case o: Fortress => FortressOps(o)
    case o: Wasp => WaspOps(o)
  }): FighterOps[_]).asInstanceOf[FighterOps[A]]
}
