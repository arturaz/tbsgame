package app.models.game.world.buildings

import app.models.game.Bot
import app.models.game.world.units.Wasp
import app.models.game.world._

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

  override def spawn(world: World, position: Vect2) = Wasp.warp(world, owner, position)
}
