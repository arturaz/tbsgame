package app.models.world.units

import app.models.world._

trait WUnitStats extends FactionObjStats with WarpableStats

/* World unit */
trait WUnit extends FactionObj with Warpable {
  override def stats: WUnitStats
  var position: Vect2
}
