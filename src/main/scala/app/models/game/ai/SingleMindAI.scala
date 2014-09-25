package app.models.game.ai

import app.algorithms.Pathfinding.SearchRes
import app.algorithms.{Combat, Pathfinding}
import app.models.Owner
import app.models.game.events.Evented
import app.models.world._
import app.models.world.units.WUnit
import implicits._
import infrastructure.Log

import scala.util.Random

/* AI for units that have no central controlling entity. */
object SingleMindAI {
  private[this] def srOrd[A](f: SearchRes[OwnedObj] => A)(implicit ord: Ordering[A])
  : Ordering[SearchRes[OwnedObj]] = Ordering.by(f)
  private[this] def fOrd[A](f: OwnedObj => A)(implicit ord: Ordering[A])
  : Ordering[OwnedObj] = Ordering.by(f)

  /* Ordering which implies worthyness of attacking a target. */
  val AttackOwnedObjOrd =
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
      case f: Fighter => f.companion.attack.end
      case _ => 0
    }.reverse

  val AttackSearchResOrdering = {
    srOrd(_.value)(AttackOwnedObjOrd) orElse
    /* Least movement required. */
    srOrd(_.movementNeeded)
  }

  /* Order by attack ordering, then if equal, by random. */
  def atkOrdRndLt[A](ord: Ordering[A])(a: A, b: A): A = ord.compare(a, b) match {
    case -1 => a
    case 0 => if (Random.nextBoolean()) a else b
    case 1 => b
  }
  def atkOrdRndLtOO(a: OwnedObj, b: OwnedObj) =
    atkOrdRndLt(AttackOwnedObjOrd)(a, b)
  def atkOrdRndLtSR(a: SearchRes[OwnedObj], b: SearchRes[OwnedObj]) =
    atkOrdRndLt(AttackSearchResOrdering)(a, b)

  /* Simulate AI actions for all units. */
  def act(world: World, owner: Owner): Combat.ActionResult = {
    val units = world.objects.
      collect { case o: WUnit with Fighter if o.owner == owner => o }
    units.foldLeft(Evented(world)) { case (curWorld, unit) =>
      act(curWorld, unit).fold(
        err => {
          Log.error(s"$owner unit $unit failed to act: $err")
          curWorld
        },
        identity
      )
    }
  }

  def act(
    world: Evented[World], unit: WUnit with Fighter
  ): Combat.EitherActionResult =
    act(world.value, unit).right.map { world.events +: _ }

  def act(world: World, unit: WUnit with Fighter): Combat.EitherActionResult = {
    val visibleTargets =
      world.objects.view.
      collect { case o: OwnedObj => o }.
      filter(o => o.isEnemy(unit) && unit.sees(o)).
      toSeq
    val obstacles = unit.obstacles(world.objects).map(_.bounds)
    val attackableTargets =
      Pathfinding.attackSearch(unit, visibleTargets, obstacles)(_.bounds).
      /* Filter out those targets which we can't inflict damage to. */
      filter(_.value.companion.defense.start <= unit.companion.attack.end)

    if (attackableTargets.isEmpty) Right(Evented(world))
    else {
      val target = attackableTargets.reduce(atkOrdRndLt(AttackSearchResOrdering))
      Combat.moveAttack(world, unit, target)
    }
  }
}
