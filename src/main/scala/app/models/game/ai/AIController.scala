package app.models.game.ai

import app.algorithms.Pathfinding
import app.algorithms.Pathfinding.SearchRes
import app.models.Player
import app.models.game.events.Event
import app.models.world.units.WUnit
import app.models.world.{FactionObj, Fighter, Warpable, World}
import implicits._

import scala.util.Random

object AIController {
  private[this] def srOrd[A](f: SearchRes[FactionObj] => A)(implicit ord: Ordering[A])
  : Ordering[SearchRes[FactionObj]] = Ordering.by(f)
  private[this] def fOrd[A](f: FactionObj => A)(implicit ord: Ordering[A])
  : Ordering[FactionObj] = Ordering.by(f)

  /* Ordering which implies worthyness of attacking a target. */
  private[this] val AttackOrdering = {
    val factionObjOrd =
      /* Fighters must be dealt with first */
      fOrd(_.isInstanceOf[Fighter]).reverse orElse
      /* Warping in objects must be dealt with first */
      fOrd {
        case w: Warpable => w.isWarpingIn
        case _ => false
      }.reverse orElse
      /* Attack ones with least HP first. */
      fOrd(_.hp) orElse
      /* Attack ones with biggest damage output first. */
      fOrd {
        case f: Fighter => f.stats.attack.end
        case _ => 0
      }.reverse

    srOrd(_.value)(factionObjOrd) orElse
    /* Least movement required. */
    srOrd(_.movementNeeded)
  }

  type ActionResult = (World, Vector[Event])

  /* Simulate AI actions for all units. */
  def act(world: World, ai: Player): ActionResult = {
    val units = world.objects.
      collect { case o: WUnit with Fighter if o.owner == ai => o}
    units.foldLeft(
      (world, Vector.empty[Event])
    ) { case ((curWorld, curEvents), unit) =>
      val (newWorld, newEvents) = act(curWorld, unit)
      (newWorld, curEvents ++ newEvents)
    }
  }

  def act(world: World, unit: WUnit with Fighter): ActionResult = {
    val team = unit.owner.team
    val visibleTargets =
      world.objects.view.
      collect { case o: FactionObj => o }.
      filter(o => o.owner.team != team && unit.sees(o)).
      toSeq
    val obstacles = world.objects.
      filter(o => unit.movementZone.exists(o.bounds.contains)).map(_.bounds)
    val attackableTargets =
      Pathfinding.attackSearch(unit, visibleTargets, obstacles)(_.bounds).
      /* Filter out those targets which we can't inflict damage to. */
      filter(_.value.stats.defense.start <= unit.stats.attack.end)

    if (attackableTargets.isEmpty) (world, Vector.empty)
    else {
      val target = attackableTargets.reduce((a, b) =>
        AttackOrdering.compare(a, b) match {
          case -1 => a
          case 0 => if (Random.nextBoolean()) a else b
          case 1 => b
        }
      )
      moveAttack(world, unit, target)
    }
  }

  def moveAttack(
    world: World, unit: WUnit with Fighter, target: SearchRes[FactionObj]
  ): ActionResult = {
    val newWorld = unit.moveTo(target.path.last).right.flatMap {
      _.attack(target.value).right.map { case (attack, attackUnit) =>
        world.update(unit, attackUnit)
      }
    }
    null
//    unit.moveTo(target.path.last).right.map { movedUnit =>
//    }
//    val (moveWorld, moveUnit) = world.update(unit, )
//    for {
//      (world, unit) <-
//      (attack, unit) <- unit.attack(target.value)
//      (world, unit) <- world.update(unit, unit.attack())
//    } yield 0
//    world.update()
//    val worldAfterMove = world.update(
//      unit,
//      unit.moveTo
//    )
//    val (attack, attackedUnit) = unit.attack(target.value)
//    val worldAfterAttack = worldAfterMove.update(unit, attackedUnit)
//
//    val moves = target.path.zipWithIndex.drop(1).map { case (v, idx) =>
//      MoveEvt(unit.id, v, unit.movementLeft.range - idx)
//    }
//    val attackEvt = AttackEvt(unit.id, target.value.id, attack)
//    val events = moves :+ attackEvt
//
//    (worldAfterAttack, events)
  }
}
