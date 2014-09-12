package app.models.world.buildings

import app.models.Owner
import app.models.world.{Warpable, Vect2, WarpableStats}

object WarpLinker extends BuildingStats with WarpableStats {
  override val maxHp: Int = 2
  override val size: Vect2 = Vect2(1, 1)
  override val warpTime: Int = 2
  override val cost: Int = 3
}

class WarpLinker(
  val position: Vect2, var owner: Owner
) extends Building with Warpable {
  override def stats = WarpLinker
}