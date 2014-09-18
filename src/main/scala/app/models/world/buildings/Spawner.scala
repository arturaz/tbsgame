package app.models.world.buildings

import app.models.Owner
import app.models.world.units.Wasp
import app.models.world.{WObject, Vect2}

object Spawner extends BuildingStats with GrowingSpawnerStats {
  override val maxHp: Int = 10
  override val size: Vect2 = Vect2(2, 2)
  val DefaultTurnsPerStrength = 6
}

case class Spawner(
  position: Vect2, owner: Owner,
  id: WObject.Id=WObject.newId,
  turnsPerStrength: Int=Spawner.DefaultTurnsPerStrength,
  turns: Int=0, hp: Int=Spawner.maxHp
) extends Building with GrowingSpawner {
  override def stats = Spawner

  override type Self = Spawner
  override type Stats = Spawner.type
  override protected def self = this
  override protected def withNewHp(hp: Int) = copy(hp = hp)
  override def withTurnsPerStrength(value: Int) = copy(turnsPerStrength = value)
  override protected def withTurns(self: Self, turns: Int) = self.copy(turns = turns)

  override def spawn(position: Vect2) = Wasp(position, owner)
}
