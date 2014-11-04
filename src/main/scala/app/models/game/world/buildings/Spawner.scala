package app.models.game.world.buildings

import app.models.game.Bot
import app.models.game.world.units.{RayShip, Wasp}
import app.models.game.world._

import scala.util.Random
import implicits._

object Spawner extends BuildingCompanion[Spawner] with SizedWObjectCompanion
with GrowingSpawnerCompanion[Spawner] {
  override val maxHp = HP(100)
  override val size: Vect2 = Vect2(2, 2)
  override val isCritical: Boolean = true
  override val defense = emptyRange
  val DefaultTurnsPerStrength = 5

  override def withNewHp(hp: HP)(self: Spawner) = self.copy(hp = hp)
  override def withTurnsPerStrength(value: Int)(self: Spawner) =
    self.copy(turnsPerStrength = value)
  override def withTurns(turns: Int)(self: Spawner) =
    self.copy(turns = turns)
}

case class Spawner(
  position: Vect2, owner: Bot,
  id: WObject.Id=WObject.newId,
  turnsPerStrength: Int=Spawner.DefaultTurnsPerStrength,
  turns: Int=0, hp: HP=Spawner.maxHp
) extends BotBuilding with GrowingSpawner with SizedWObject {
  type Self = Spawner
  def self = this
  override def companion = Spawner
  override type Companion = Spawner.type

  override def spawn(world: World, position: Vect2) = {
    case class Counter(movable: Int, immovable: Int) {
      lazy val total = movable + immovable
      def waspChance = percent(movable)
      def rayShipChance = percent(immovable)
      private[this] def percent(p: Double) = if (total == 0) 0.5 else p / total
    }

    val counts = world.objects.view.collect {
      case o: OwnedObj with Fighter if o.isEnemy(this) => o
    }.foldLeft(Counter(0, 0)) {
      case (cnt, obj: MovableWObject) => cnt.copy(movable = cnt.movable + 1)
      case (cnt, obj) => cnt.copy(immovable = cnt.immovable + 1)
    }
    val warpable = if (Random.chance(counts.waspChance)) Wasp else RayShip
    warpable.warp(world, owner, position)
  }
}
