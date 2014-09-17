package app.models.game.ai

import app.algorithms.Pathfinding
import app.algorithms.Pathfinding.SearchRes
import app.models.Player
import app.models.game.events.{AttackEvt, MoveEvt, Event}
import app.models.world.units.WUnit
import app.models.world.{FactionObj, Fighter, Warpable, World}
import implicits._
import infrastructure.Log

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
  private[this] type EitherActionResult = Either[String, ActionResult]

  /* Simulate AI actions for all units. */
  def act(world: World, ai: Player): ActionResult = {
    val units = world.objects.
      collect { case o: WUnit with Fighter if o.owner == ai => o}
    units.foldLeft(
      (world, Vector.empty[Event])
    ) { case ((curWorld, curEvents), unit) =>
      act(curWorld, unit).fold(
        err => {
          Log.error(s"$ai unit $unit failed to act: $err")
          (curWorld, Vector.empty)
        },
        {case (newWorld, newEvents) => (newWorld, curEvents ++ newEvents)}
      )
    }
  }

  def act(world: World, unit: WUnit with Fighter): EitherActionResult = {
    val team = unit.owner.team
    val visibleTargets =
      world.objects.view.
      collect { case o: FactionObj => o }.
      filter(o => o.owner.team != team && unit.sees(o)).
      toSeq
    val obstacles = unit.obstacles(world.objects).map(_.bounds)
    val attackableTargets =
      Pathfinding.attackSearch(unit, visibleTargets, obstacles)(_.bounds).
      /* Filter out those targets which we can't inflict damage to. */
      filter(_.value.stats.defense.start <= unit.stats.attack.end)

    if (attackableTargets.isEmpty) Right(world, Vector.empty)
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
  ): EitherActionResult = {
    unit.moveTo(target.path.last).right.flatMap {
      _.attack(target.value).right.map { case (attack, attackUnit) => (
        world.update(unit, attackUnit).update(attack, target.value),
        attack
      ) }
    }.right.map { case (newWorld, attack) =>
      val moves = target.path.zipWithIndex.drop(1).map { case (v, idx) =>
        MoveEvt(unit.id, v, unit.movementLeft.range - idx)
      }
      val attackEvt = AttackEvt(unit.id, target.value.id, attack)
      val events = moves :+ attackEvt

      (newWorld, events)
    }
  }
}
