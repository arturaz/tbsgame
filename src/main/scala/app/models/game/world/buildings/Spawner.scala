package app.models.game.world.buildings

import app.models.game.{Actions, Bot}
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
  val StrengthReductionPerAction = SpawnerStr(4)
  val DefaultStartingStrength = SpawnerStr(2 + StrengthReductionPerAction.value * 3)
  val DefaultTurnsPerStrength = SpawnerStr(5)

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

  def strength = startingStrength + SpawnerStr(turns) / turnsPerStrength
  def strength(actions: Actions): SpawnerStr =
    strength - stats.StrengthReductionPerAction * SpawnerStr(actions.value)
  val startingStrength: SpawnerStr
  val turnsPerStrength: SpawnerStr

  /* Try to spawn at position, returning Right(None) if unit was killed after spawn. */
  def spawn(world: World, position: Vect2) = {
    case class Counter(counts: Map[WObjKind, Int]=Map.empty.withDefaultValue(0)) {
      def +(kind: WObjKind) = Counter(counts updated (kind, counts(kind) + 1))

      lazy val total = counts.values.sum
      lazy val warpableWeights = stats.Spawnables.map {
        case (kind, w) => w -> counts(kind)
      }

      def randomWarpable = {
        if (total == 0) stats.Spawnables.random.get._2
        else warpableWeights.weightedRandom.get
      }
    }

    val counts = world.objects.foldLeft(Counter()) {
      case (cnt, o: OwnedObj) if o.isEnemy(this) => cnt + o.stats.kind
      case (cnt, _) => cnt
    }
    val warpable = counts.randomWarpable
    warpable.warp(world, owner, position)
  }
}
