package app.models.game.world.buildings

import akka.event.LoggingAdapter
import app.models.game.Actions
import app.models.game.world._
import app.models.game.world.units._
import implicits._

trait SpawnerStatsImpl { _: SpawnerStats.type =>
  override val maxHp = HP(1200)
  override val size = Vect2(2, 2)
  override val isCritical = true
  override val warpGiven = RectDistance(1)
  override val actionsGiven = Actions(2)
  override val specialResourcesGiven = Resources(2)
  override val specialActionsNeeded = Actions(1)
}

case class SpawnerOps(self: Spawner) extends OwnedObjOps[Spawner]
with TurnCounterOps[Spawner] {
  override def withNewHp(hp: HP) = self.copy(hp = hp)
  override def withTurns(turns: Int) = self.copy(turns = turns)
}

trait SpawnerImpl { _: Spawner =>
  type Stats = SpawnerStats.type
  override val stats = SpawnerStats
}
