package app.models.game.world

import akka.event.LoggingAdapter
import app.models.game.ai.SingleMindAI
import app.models.game.events.Evented
import implicits._

trait ReactiveFighterOps[Self <: ReactiveFighter] extends FighterOps[Self] {
  def shouldReact(self: OwnedObj): Boolean = self.isWarpedIn
  def shouldReact(self: OwnedObj, target: OwnedObj): Boolean =
    shouldReact(self) && self.isEnemy(target) && target.isInstanceOf[MovableWObject]

  def attackReachable(
    data: WObject.WorldObjUpdate[Self]
  )(implicit log: LoggingAdapter): WObject.WorldObjUpdate[Self] =
    data.flatMap { case orig @ (world, self) =>
      if (shouldReact(self)) {
        val targets = world.objects.collect {
          case obj: OwnedObj if shouldReact(self, obj) && self.canAttack(obj, world) => obj
        }
        if (targets.isEmpty) data
        else {
          val target = targets.reduce(SingleMindAI.atkOrdRndLtOO(self))
          self.attackWS(target, world).fold(
            err => {
              log.error(
                s"{} tried to attack reachable {}, but failed: {}", self, target, err
              )
              data
            },
            _.map { case (w, s) => (w, s.asInstanceOf[Self])}
          )
        }
      }
      else data
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
    super.teamTurnFinishedSelf(world) |> resetAttack |> companion.attackReachable

  /* Attacks the target if he can. Returns None if there was no reaction. */
  def reactToOpt[A <: OwnedObj](
    obj: A, world: World
  ): Option[Reaction[WObject.WorldObjOptUpdate[A]]] =
    if (companion.shouldReact(self, obj))
      attack(obj, world).right.toOption.map {
        case evt @ Evented((newWorld, _, attack, newObj), _) =>
          Reaction(evt.copy(value = (newWorld, newObj)), abortReacting = newObj.isEmpty)
      }
    else None

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
