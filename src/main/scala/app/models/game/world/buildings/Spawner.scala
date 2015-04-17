package app.models.game.world.buildings

import akka.event.LoggingAdapter
import app.models.game.Actions
import app.models.game.world._
import app.models.game.world.units._
import implicits._

object SpawnerStats extends BuildingStats with SizedWObjectStats {
  override val maxHp = HP(1200)
  override val size = Vect2(2, 2)
  override val isCritical = true
  override val kind = WObjKind.Heavy
  override val warpGiven = visibility

  /* Each action from buildings that the team controls reduces spawner strength by this
     much. This makes spawner get angrier as VPTowers are taken from it. */
  val StrengthReductionPerAction = SpawnerStr(0)
  val DefaultStartingStrength = SpawnerStr(4)
  val DefaultTurnsPerStrength = Option.empty[SpawnerStr]

  val Spawnables = IndexedSeq(
    WObjKind.Light -> WaspStats,
    WObjKind.Medium -> FortressStats,
    WObjKind.Heavy -> RayShipStats
  )
}

case class SpawnerOps(self: Spawner) extends OwnedObjOps[Spawner]
with TurnCounterOps[Spawner] {
  override def withNewHp(hp: HP) = self.copy(hp = hp)
  override def withTurns(turns: Int) = self.copy(turns = turns)
}

trait SpawnerImpl { _: Spawner =>
  type Stats = SpawnerStats.type
  override val stats = SpawnerStats

  def strength =
    startingStrength + turnsPerStrength.fold2(SpawnerStr(0), SpawnerStr(turns) / _)
  def strength(actions: Actions): SpawnerStr =
    strength - stats.StrengthReductionPerAction * SpawnerStr(actions.value)
  val startingStrength: SpawnerStr
  val turnsPerStrength: Option[SpawnerStr]

  /* Try to spawn at position, returning Right(None) if unit was killed after spawn. */
  def spawn(world: World, position: Vect2)(implicit log: LoggingAdapter) = {
//    case class Counter(counts: Map[WObjKind, Int]=Map.empty.withDefaultValue(0)) {
//      def +(kind: WObjKind) = Counter(counts updated (kind, counts(kind) + 1))
//
//      lazy val total = counts.values.sum
//      lazy val warpableWeights = stats.Spawnables.map {
//        case (kind, w) => w -> counts(kind)
//      }
//
//      def randomWarpable = {
//        if (total == 0) stats.Spawnables.random.get._2
//        else warpableWeights.weightedRandom.get
//      }
//    }
//
//    val counts = world.objects.foldLeft(Counter()) {
//      case (cnt, o: OwnedObj) if o.isEnemy(this) => cnt + o.stats.kind
//      case (cnt, _) => cnt
//    }

    val warpable = stats.Spawnables.random.get._2
    warpable.warp(world, owner, position)
  }
}