package app.models.world.buildings

import app.models.Owner
import app.models.world.Vect2

object WarpGate extends BuildingStats {
  override val maxHp: Int = 35
  override val size: Vect2 = Vect2(6, 4)
}

class WarpGate(val position: Vect2, var owner: Owner) extends Building {
  override def stats = WarpGate
}
