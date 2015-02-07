package app.models.game.world

import akka.event.LoggingAdapter
import implicits._

import scala.language.implicitConversions

trait GivingVictoryPointsStats extends OwnedObjStats {
  /* Victory points given each turn */
  val vpsGiven: VPS
}

trait GivingVictoryPointsImpl extends OwnedObjImpl {
  val stats: GivingVictoryPointsStats
}

object GivingVictoryPoints extends ToGivingVictoryPointsOps

trait GivingVictoryPointsOps[Self <: GivingVictoryPoints] {
  def self: Self
  def giveVP(world: World) = world.addVps(self.owner, self.stats.vpsGiven)

  def teamTurnStarted(world: World) = giveVP(world)
}

trait ToGivingVictoryPointsOps {
  implicit def toGivingVictoryPointsOps[A <: GivingVictoryPoints](a: A)
  : GivingVictoryPointsOps[A] = (a match {

  }).asInstanceOf[GivingVictoryPointsOps[A]]
}