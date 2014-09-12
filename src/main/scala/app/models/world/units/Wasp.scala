package app.models.world.units

import app.models.Owner
import app.models.world.{Vect2, FighterStats, Fighter}

object Wasp extends WUnitStats with FighterStats {
  override val attack: Range = 1 to 6
  override val defense: Range = 2 to 7
  override val attackRange: Int = 3
  override val visibility: Int = 4
  override val maxHp: Int = 1
  override val size: Vect2 = Vect2.one
  override val warpTime: Int = 0
  override val cost: Int = 0
}

class Wasp(
  var position: Vect2, var owner: Owner
) extends WUnit with Fighter {
  override def stats = Wasp
}
