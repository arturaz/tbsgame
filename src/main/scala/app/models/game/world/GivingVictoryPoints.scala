package app.models.game.world

import akka.event.LoggingAdapter
import implicits._

trait GivingVictoryPointsOps[Self <: GivingVictoryPoints] extends OwnedObjOps[Self] {
  def giveVP(data: WObject.WorldObjUpdate[Self]) = data.map { case (world, self) =>
    val newWorld = world.addVps(self.owner, self.companion.vpsGiven)
    (newWorld, self)
  }
}

trait GivingVictoryPointsStats extends OwnedObjStats {
  /* Victory points given each turn */
  val vpsGiven: VPS
}

trait GivingVictoryPointsCompanion[Self <: GivingVictoryPoints]
extends GivingVictoryPointsOps[Self] with GivingVictoryPointsStats

trait GivingVictoryPoints extends OwnedObj {
  type Self <: GivingVictoryPoints
  type Companion <: GivingVictoryPointsStats with GivingVictoryPointsOps[Self]

  override def teamTurnStartedSelf(world: World)(implicit log: LoggingAdapter) =
    super.teamTurnStartedSelf(world) |> companion.giveVP
}
