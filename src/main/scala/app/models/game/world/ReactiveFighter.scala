package app.models.game.world

import akka.event.LoggingAdapter
import app.models.game.ai.SingleMindAI
import app.models.game.events.Evented
import app.models.game.world.buildings.LaserTowerOps
import implicits._

import scala.annotation.tailrec
import scala.language.implicitConversions
import app.models.game.world.Ops._

case class Reaction[A](value: A, abortReacting: Boolean) {
  def map[B](f: A => B) = copy(value = f(value))
}

trait ReactiveFighterImpl extends FighterImpl { self: Fighter =>
  def shouldReact: Boolean =
    isWarpedIn && attacksLeft.isNotZero

  def shouldReact(target: OwnedObj): Boolean =
    shouldReact && isEnemy(target) && target.isInstanceOf[Movable]

  /* Attacks the target if he can. Returns None if there was no reaction. */
  def reactToOpt[A <: OwnedObj](
    obj: A, world: World
  ): Option[Reaction[WObject.WorldObjOptUpdate[A]]] = {
    if (shouldReact(obj))
      toFighterOps(this).attack(obj, world).right.toOption.map {
        case evt @ Evented((newWorld, newSelf, attack, newObjOpt), _) =>
          def thisReaction = Reaction(
            evt.map(_ => (newWorld, newObjOpt)), abortReacting = newObjOpt.isEmpty
          )
          newObjOpt.fold2(
            thisReaction,
            newObj => newSelf.reactToOpt(newObj, newWorld).fold2(
              thisReaction,
              _.map(evt.events ++: _)
            )
          )
      }
    else None
  }

  /* Attacks the target if he can. */
  def reactTo[A <: OwnedObj](
    obj: A, world: World
  ): Reaction[WObject.WorldObjOptUpdate[A]] =
    reactToOpt(obj, world).fold2(
      Reaction(Evented((world, Some(obj))), abortReacting = false),
      identity
    )
}

trait ReactiveFighterOps[Self <: ReactiveFighter] {
  def self: Self

  /* Needed to be able to react to other player actions. */
  def teamTurnFinished(world: World)(implicit log: LoggingAdapter) =
    self.attackReachableWhileHasAttacks(world)(log.prefixed("react|"))

  def attackReachable
  (world: World)(implicit log: LoggingAdapter): WObject.WorldObjUpdate[Self] = {
    def orig = Evented((world, self))

    if (self.shouldReact) {
      val targets = world.objects.collect {
        case obj: OwnedObj
          if self.shouldReact(obj) && self.canAttack(obj, world)
        => obj
      }
      if (targets.isEmpty) {
        log.debug("no targets to react to")
        orig
      }
      else {
        val target = targets.reduce(SingleMindAI.atkOrdRndLtOO(self))
        self.attackWS(target, world).fold(
          err => {
            log.error(
              s"{} tried to attack reachable {}, but failed: {}",
              self, target, err
            )
            orig
          },
          newData => {
            log.debug("reacted against {}", target)
            newData
          }
        )
      }
    }
    else {
      log.debug("not reacting because we shouldn't")
      orig
    }
  }

  def attackReachableWhileHasAttacks
  (world: World)(implicit log: LoggingAdapter): WObject.WorldObjUpdate[Self] = {
    def rec(data: Evented[(World, Self)]): Evented[(World, Self)] = {
      val newData = data.value._2.attackReachable(world)

      if (newData.events.isEmpty) {
        log.debug("nothing happened")
        data
      }
      else data.events ++: rec(newData)
    }

    rec(Evented((world, self)))
  }
}

trait ToReactiveFighterOps {
  implicit def toReactiveFighterOps[A <: ReactiveFighter](a: A)
  : ReactiveFighterOps[A] = (a match {
    case o: LaserTower => LaserTowerOps(o)
  }).asInstanceOf[ReactiveFighterOps[A]]
}
