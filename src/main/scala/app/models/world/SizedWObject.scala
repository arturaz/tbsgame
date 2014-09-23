package app.models.world

trait SizedWObjectOps extends WObjectOps {
_: SizedWObjectStats =>
  override def bounds(position: Vect2) = Bounds(position, size)
}

/* Objects which have size cannot usually be moved. */
trait SizedWObjectStats extends WObjectStats {
  val size: Vect2
}

trait SizedWObjectCompanion
extends SizedWObjectOps with SizedWObjectStats

trait SizedWObject extends WObject with Mobility[Mobility.Static.type] {
  type Companion <: SizedWObjectOps with SizedWObjectStats
}
