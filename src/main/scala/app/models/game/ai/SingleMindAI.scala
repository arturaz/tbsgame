package app.models.game.ai

import akka.event.LoggingAdapter
import app.algorithms.Pathfinding.SearchRes
import app.algorithms.{Combat, Pathfinding}
import app.models.game.{Attack, Owner}
import app.models.game.events.Evented
import app.models.game.world._
import app.models.game.world.units.WUnit
import implicits._
import infrastructure.PrefixedLoggingAdapter

import scala.util.Random

/* AI for units that have no central controlling entity. */
object SingleMindAI {
  private[this] val LogPrefix = "SingleMindAI|"

  private[this] def srOrd[A](f: SearchRes[OwnedObj] => A)(implicit ord: Ordering[A])
  : Ordering[SearchRes[OwnedObj]] = Ordering.by(f)
  private[this] def fOrd[A](f: OwnedObj => A)(implicit ord: Ordering[A])
  : Ordering[OwnedObj] = Ordering.by(f)

  /* Ordering which implies worthyness of attacking a target. */
  def AttackOwnedObjOrd(self: Fighter) =
    /* Filter out those targets which we can't inflict damage to. */
    fOrd { o =>
      Attack(
        self.companion.attack.end, self.companion.kind.multiplierAt(o.companion.kind),
        None, o.companion.defense.start
      ).successful
    }.reverse orElse
    /* Fighters must be dealt with first */
    fOrd(_.isInstanceOf[Fighter]).reverse orElse
    /* Attack ones where most damage can be done. */
    fOrd(o => self.companion.kind.multiplierAt(o.companion.kind)).reverse orElse
    /* Attack ones with least HP first. */
    fOrd(_.hp) orElse
    /* Warping in objects must be dealt with first */
    fOrd {
      case w: Warpable => w.isWarpingIn
      case _ => false
    }.reverse orElse
    /* Attack ones with biggest damage output first. */
    fOrd {
      case f: Fighter => f.companion.attack.end
      case _ => 0
    }.reverse

  def AttackSearchResOrdering(self: Fighter) = {
    srOrd(_.value)(AttackOwnedObjOrd(self)) orElse
    /* Least movement required. */
    srOrd(_.movementNeeded)
  }

  /* Order by attack ordering, then if equal, by random. */
  def atkOrdRndLt[A](ord: Ordering[A])(a: A, b: A): A = ord.compare(a, b) match {
    case -1 => a
    case 0 => if (Random.nextBoolean()) a else b
    case 1 => b
  }
  def atkOrdRndLtOO(self: Fighter)(a: OwnedObj, b: OwnedObj) =
    atkOrdRndLt(AttackOwnedObjOrd(self))(a, b)
  def atkOrdRndLtSR(self: Fighter)(a: SearchRes[OwnedObj], b: SearchRes[OwnedObj]) =
    atkOrdRndLt(AttackSearchResOrdering(self))(a, b)

  /* Simulate AI actions for all units. */
  def act(world: World, owner: Owner)(implicit log: LoggingAdapter)
  : Combat.RawWorldResult = {
    val units = world.objects.
      collect { case o: WUnit with Fighter if owner === o.owner => o }
    units.foldLeft(Evented(world)) { case (curWorld, unit) =>
      act(curWorld, unit).fold(
        err => {
          log.error(s"$LogPrefix{} unit {} failed to act: {}", owner, unit, err)
          curWorld
        },
        identity
      )
    }
  }

  def act(
    world: Evented[World], unit: WUnit with Fighter
  )(implicit log: LoggingAdapter): Combat.WorldResult =
    act(world.value, unit).right.map { world.events ++: _ }

  def act(world: World, unit: WUnit with Fighter)(implicit log: LoggingAdapter)
  : Combat.WorldResult = {
    whileHasAttacksLeft(world, unit)((world, unit) => {
      val visibleTargets =
        world.objects.view.
          collect { case o: OwnedObj => o }.
          filter(o => o.isEnemy(unit) && unit.sees(o)).
          toSeq

      findAndMoveAttackTarget(world, visibleTargets, unit)
    })
  }

  def findTarget(
    world: World, targets: Iterable[OwnedObj], unit: WUnit with Fighter
  )(implicit log: LoggingAdapter): Option[SearchRes[OwnedObj]] = {
    if (targets.isEmpty) return None

    val attackableTargets =
      Pathfinding.attackSearch(unit, targets, world.bounds, world.objects)(_.bounds)

    if (attackableTargets.isEmpty) {
      log.debug(
        s"${LogPrefix}findTarget: no attackable targets from {} for {}", targets, unit
      )
      None
    }
    else {
      val target = attackableTargets.reduce(atkOrdRndLt(AttackSearchResOrdering(unit)))
      log.debug(
        s"${LogPrefix}findTarget: attackable target = {} from {} for {}",
        target, targets, unit
      )
      Some(target)
    }
  }

  def findAndMoveAttackTarget(
    world: World, targets: Iterable[OwnedObj], unit: WUnit with Fighter
  )(implicit log: LoggingAdapter) = findTarget(world, targets, unit).map { target =>
    log.debug("found target for {}: {}", unit, target)
    Combat.moveAttack(world, unit, target)
  }

  def whileHasAttacksLeft[A <: WUnit with Fighter](world: World, unit: A)(
    doAct: (World, A) => Option[Combat.Result[A]],
    onNoTarget: (Evented[World], A) => Combat.WorldResult =
      (newWorld: Evented[World], _: A) => newWorld.right[String]
  ): Combat.WorldResult = {
    var newWorld = Evented(world)
    var newUnitOpt = Option(unit)
    do {
      val newUnit = newUnitOpt.get
      doAct(newWorld.value, newUnit).fold2(
        // No more things to do.
        return onNoTarget(newWorld, newUnit),
        {
          case Left(err) => return err.left[Evented[World]]
          case Right(evented) =>
            newWorld = newWorld.events ++: evented.map { case (_world, unitOpt) =>
              newUnitOpt = unitOpt
              _world
            }
        }
      )
    } while (newUnitOpt.exists(_.attacksLeft.isNotZero))

    newWorld.right[String]
  }
}
