package app.models.world.buildings

import app.models.Bot
import app.models.world.units.Wasp
import app.models.world.{Vect2, WObject, World}

object Spawner extends BuildingCompanion[Spawner] with GrowingSpawnerCompanion[Spawner] {
  override val maxHp: Int = 10
  override val size: Vect2 = Vect2(2, 2)
  override val isCritical: Boolean = true
  val DefaultTurnsPerStrength = 6

  override def withNewHp(hp: Int)(self: Spawner) = self.copy(hp = hp)
  override def withTurnsPerStrength(value: Int)(self: Spawner) =
    self.copy(turnsPerStrength = value)
  override def withTurns(turns: Int)(self: Spawner) =
    self.copy(turns = turns)
}

case class Spawner(
  position: Vect2, owner: Bot,
  id: WObject.Id=WObject.newId,
  turnsPerStrength: Int=Spawner.DefaultTurnsPerStrength,
  turns: Int=0, hp: Int=Spawner.maxHp
) extends BotBuilding with GrowingSpawner {
  override def companion = Spawner
  override type Companion = Spawner.type

  override def spawn(world: World, position: Vect2) = Wasp.warp(world, owner, position)
}
