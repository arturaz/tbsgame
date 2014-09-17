package app.models.world.buildings

import app.models.Owner
import app.models.world.{WObject, Vect2}

object Spawner extends BuildingStats {
  override val maxHp: Int = 10
  override val size: Vect2 = Vect2(2, 2)
}

case class Spawner(
  id: WObject.Id, position: Vect2, owner: Owner,
  hp: Int=Spawner.maxHp
) extends Building {
  override def stats = Spawner

  override type Self = Spawner
  override type Stats = Spawner.type
  override protected def self = this
  override protected def withNewHp(hp: Int) = copy(hp = hp)
}
