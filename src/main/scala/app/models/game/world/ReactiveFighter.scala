package app.models.game.world

import akka.event.LoggingAdapter
import app.models.game.ai.SingleMindAI
import app.models.game.events.Evented
import implicits._

import scala.annotation.tailrec

trait ReactiveFighterOps[Self <: ReactiveFighter] extends FighterOps[Self] {
  def shouldReact(self: ReactiveFighter): Boolean =
    self.isWarpedIn && self.attacksLeft.isNotZero
  def shouldReact(self: ReactiveFighter, target: OwnedObj): Boolean =
    shouldReact(self) && self.isEnemy(target) && target.isInstanceOf[Movable]

  def attackReachable(
    data: WObject.WorldObjUpdate[Self]
  )(implicit log: LoggingAdapter): WObject.WorldObjUpdate[Self] =
    data.flatMap { case orig @ (world, self) =>
      if (shouldReact(self)) {
        val targets = world.objects.collect {
          case obj: OwnedObj if shouldReact(self, obj) && self.canAttack(obj, world) => obj
        }
        if (targets.isEmpty) {
          log.debug("no targets to react to")
          Evented(orig)
        }
        else {
          val target = targets.reduce(SingleMindAI.atkOrdRndLtOO(self))
          self.attackWS(target, world).fold(
            err => {
              log.error(
                s"{} tried to attack reachable {}, but failed: {}", self, target, err
              )
              Evented(orig)
            },
            newData => {
              log.debug("reacted against {}", target)
              newData.map { case (w, s) => (w, s.asInstanceOf[Self])}
            }
          )
        }
      }
      else {
        log.debug("not reacting because we shouldn't")
        Evented(orig)
      }
    }

  @tailrec final def attackReachableWhileHasAttacks(
    data: WObject.WorldObjUpdate[Self]
  )(implicit log: LoggingAdapter): WObject.WorldObjUpdate[Self] = {
    val newData = attackReachable(data)

    if (data.events.size == newData.events.size) {
      log.debug("nothing happened")
      newData
    }
    else attackReachableWhileHasAttacks(newData)
  }
}

trait ReactiveFighterStats extends FighterStats

trait ReactiveFighterCompanion[Self <: ReactiveFighter] extends ReactiveFighterOps[Self]
with ReactiveFighterStats

trait ReactiveFighter extends Fighter { traitSelf =>
  type Self >: traitSelf.type <: ReactiveFighter
  type Companion <: ReactiveFighterOps[Self] with ReactiveFighterStats

  /* Needed to be able to react to other player actions. */
  override def teamTurnFinishedSelf(world: World)(implicit log: LoggingAdapter) =
    super.teamTurnFinishedSelf(world) |> {
      (w: WObject.WorldObjUpdate[Self]) =>
        companion.attackReachableWhileHasAttacks(w)(log.prefixed("react|"))
    }

  /* Attacks the target if he can. Returns None if there was no reaction. */
  def reactToOpt[A <: OwnedObj](
    obj: A, world: World
  ): Option[Reaction[WObject.WorldObjOptUpdate[A]]] = {
    if (companion.shouldReact(self, obj))
      attack(obj, world).right.toOption.map {
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

case class Reaction[A](value: A, abortReacting: Boolean) {
  def map[B](f: A => B) = copy(value = f(value))
}
