package app.models.game.world.buildings

import akka.event.LoggingAdapter
import app.models.game.world._
import app.models.game.{Population, Player, Actions}
import implicits._

trait WarpGateStatsImpl { _: WarpGateStats.type =>
  override val maxHp = HP(450)
  override val size = Vect2(3, 3)
  override val isCritical = true
  override val actionsGiven = Actions(4)
  override val populationGiven = Population(6)
  override val visibility = RectDistance(4)
  override val warpGiven = RectDistance(2)
  override val specialActionsNeeded = Actions(1)
  override val blocksVisibility = true
  override val specialResourcesGiven = Resources(1)
  override val kind = WObjKind.Heavy
}

trait WarpGateImpl extends OwnedObjImpl with SpecialActionGetResourcesImpl {
self: WarpGate =>
}

case class WarpGateOps(self: WarpGate) extends OwnedObjOps[WarpGate] {
  override protected def withNewHp(hp: HP) = self.copy(hp = hp)
}