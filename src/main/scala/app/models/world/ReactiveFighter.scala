package app.models.world

import app.models.game.ai.SingleMindAI
import implicits._
import infrastructure.Log

trait ReactiveFighterOps[Self <: ReactiveFighter] extends FighterOps[Self] {
  def attackReachable(
    data: WObject.WorldSelfUpdate[Self]
  ): WObject.WorldSelfUpdate[Self] =
    data.flatMap { case orig @ (world, self) =>
      val targets = world.objects.collect {
        case obj: OwnedObj if
          obj.owner.isEnemyOf(self.owner) && world.isVisibleFor(self.owner, obj.bounds) &&
          self.canAttack(obj)
        => obj
      }
      if (targets.isEmpty) data
      else {
        val target = targets.reduce(SingleMindAI.atkOrdRndLtOO)
        self.attackWS(target, world).fold(
          err => {
            Log.error(s"$self tried to attack reachable $target, but failed: $err")
            data
          },
          _.asInstanceOf[WObject.WorldSelfUpdate[Self]] /* Type system hack. */
        )
      }
    }
}

trait ReactiveFighterStats extends FighterStats {
  override val moveAttackActionsNeeded: Int = 2
}

trait ReactiveFighterCompanion[Self <: ReactiveFighter] extends ReactiveFighterOps[Self]
with ReactiveFighterStats

trait ReactiveFighter extends Fighter {
  type Self <: ReactiveFighter
  type Companion <: ReactiveFighterOps[Self] with ReactiveFighterStats

  /* Needed to be able to react to other player actions. */
  override def teamTurnFinishedSelf(world: World) =
    super.teamTurnFinishedSelf(world) |> resetAttack |> companion.attackReachable
}
