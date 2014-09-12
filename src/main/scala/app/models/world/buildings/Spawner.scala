package app.models.world.buildings

import app.models.Owner
import app.models.world.Vect2

object Spawner extends BuildingStats {
  override val maxHp: Int = 10
  override val size: Vect2 = Vect2(2, 2)
}

class Spawner(val position: Vect2, var owner: Owner) extends Building {
  override def stats = Spawner
}
