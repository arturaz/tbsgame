package app.models.world.buildings

import app.models.Player
import app.models.world.units.Wasp
import app.models.world.{Vect2, WObject}

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
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId,
  turnsPerStrength: Int=Spawner.DefaultTurnsPerStrength,
  turns: Int=0, hp: Int=Spawner.maxHp
) extends PlayerBuilding with GrowingSpawner {
  override def companion = Spawner
  override type Self = Spawner
  override type Companion = Spawner.type
  override protected def self = this

  override def spawn(position: Vect2) = Wasp(position, owner)
}
