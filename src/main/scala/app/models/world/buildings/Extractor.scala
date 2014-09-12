package app.models.world.buildings

import app.models.Owner
import app.models.world.{Warpable, WarpableStats, Vect2}
import app.models.world.props.Asteroid

object Extractor extends BuildingStats with WarpableStats {
  override val maxHp = 1
  override val size: Vect2 = Vect2(1, 1)
  override val warpTime: Int = 1
  override val cost: Int = 4
}

class Extractor(
  val position: Vect2, val asteroid: Asteroid, var owner: Owner
) extends Building with Warpable {
  override def stats = Extractor
}
