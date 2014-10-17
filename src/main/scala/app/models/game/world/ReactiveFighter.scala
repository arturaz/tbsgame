package app.models.game.world

import app.models.game.Actions
import app.models.game.ai.SingleMindAI
import app.models.game.events.Evented
import implicits._
import infrastructure.Log

trait ReactiveFighterOps[Self <: ReactiveFighter] extends FighterOps[Self] {
  def attackReachable(
    data: WObject.WorldObjUpdate[Self]
  ): WObject.WorldObjUpdate[Self] =
    data.flatMap { case orig @ (world, self) =>
      val targets = world.objects.collect {
        case obj: OwnedObj if self.canAttack(obj, world) => obj
      }
      if (targets.isEmpty) data
      else {
        val target = targets.reduce(SingleMindAI.atkOrdRndLtOO)
        self.attackWS(target, world).fold(
          err => {
            Log.error(s"$self tried to attack reachable $target, but failed: $err")
            data
          },
          _.map { case (w, s) => (w, s.asInstanceOf[Self]) }
        )
      }
    }
}

trait ReactiveFighterStats extends FighterStats {
  override val moveAttackActionsNeeded = Actions(2)
}

trait ReactiveFighterCompanion[Self <: ReactiveFighter] extends ReactiveFighterOps[Self]
with ReactiveFighterStats

trait ReactiveFighter extends Fighter {
  type Self <: ReactiveFighter
  type Companion <: ReactiveFighterOps[Self] with ReactiveFighterStats

  /* Needed to be able to react to other player actions. */
  override def teamTurnFinishedSelf(world: World) =
    super.teamTurnFinishedSelf(world) |> resetAttack |> companion.attackReachable

  /* Attacks the target if he can. Returns None if there was no reaction. */
  def reactToOpt[A <: OwnedObj](
    obj: A, world: World
  ): Option[Reaction[WObject.WorldObjOptUpdate[A]]] =
    attack(obj, world).right.toOption.map { 
      case evt @ Evented((newWorld, _, attack, newObj), _) =>
        Reaction(evt.copy(value = (newWorld, newObj)), abortReacting = newObj.isEmpty)
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
