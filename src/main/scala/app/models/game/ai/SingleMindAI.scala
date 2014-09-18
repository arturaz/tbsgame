package app.models.game.ai

import app.algorithms.Pathfinding.SearchRes
import app.algorithms.{Combat, Pathfinding}
import app.models.Player
import app.models.game.events.Event
import app.models.world._
import app.models.world.units.WUnit
import implicits._
import infrastructure.Log

import scala.util.Random

/* AI for units that have no central controlling entity. */
object SingleMindAI {
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

  /* Simulate AI actions for all units. */
  def act(world: World, ai: Player): Combat.ActionResult = {
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

  def act(world: World, unit: WUnit with Fighter): Combat.EitherActionResult = {
    val visibleTargets =
      world.objects.view.
      collect { case o: FactionObj => o }.
      filter(o => o.isEnemy(unit) && unit.sees(o)).
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
      Combat.moveAttack(world, unit, target)
    }
  }
}
