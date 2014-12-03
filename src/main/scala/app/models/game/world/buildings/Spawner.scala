package app.models.game.world.buildings

import app.models.game.Bot
import app.models.game.world._
import app.models.game.world.units.{Fortress, RayShip, Wasp}
import implicits._

object Spawner extends BuildingCompanion[Spawner] with SizedWObjectCompanion
with GrowingSpawnerCompanion[Spawner] {
  override val maxHp = HP(250)
  override val size: Vect2 = Vect2(2, 2)
  override val isCritical: Boolean = true
  override val defense = emptyRange
  val DefaultStartingStrength = SpawnerStr(2)
  val DefaultTurnsPerStrength = SpawnerStr(5)
  val kind = WObjKind.Heavy

  override def withNewHp(hp: HP)(self: Spawner) = self.copy(hp = hp)
  override def withTurnsPerStrength(value: SpawnerStr)(self: Spawner) =
    self.copy(turnsPerStrength = value)
  override def withTurns(turns: Int)(self: Spawner) =
    self.copy(turns = turns)

  val Spawnables = IndexedSeq(
    WObjKind.Light -> Wasp,
    WObjKind.Medium -> Fortress,
    WObjKind.Heavy -> RayShip
  )
}

case class Spawner(
  position: Vect2, owner: Bot,
  id: WObject.Id=WObject.newId,
  startingStrength: SpawnerStr=Spawner.DefaultStartingStrength,
  turnsPerStrength: SpawnerStr=Spawner.DefaultTurnsPerStrength,
  turns: Int=0, hp: HP=Spawner.maxHp
) extends BotBuilding with GrowingSpawner with SizedWObject {
  type Self = Spawner
  def self = this
  override def companion = Spawner
  override type Companion = Spawner.type

  override def spawn(world: World, position: Vect2) = {
    case class Counter(counts: Map[WObjKind, Int]=Map.empty.withDefaultValue(0)) {
      def +(kind: WObjKind) = Counter(counts updated (kind, counts(kind) + 1))

      lazy val total = counts.values.sum
      lazy val warpableWeights = Spawner.Spawnables.map {
        case (kind, warpable) => warpable -> counts(kind)
      }

      def randomWarpable = {
        if (total == 0) Spawner.Spawnables.random.get._2
        else warpableWeights.weightedRandom.get
      }
    }

    val counts = world.objects.foldLeft(Counter()) {
      case (cnt, o: OwnedObj) if o.isEnemy(this) => cnt + o.companion.kind
      case (cnt, _) => cnt
    }
    val warpable = counts.randomWarpable
    warpable.warp(world, owner, position)
  }
}
