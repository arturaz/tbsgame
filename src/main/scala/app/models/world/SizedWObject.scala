package app.models.world

/* Objects which have size cannot usually be moved. */
trait SizedWObjectStats extends WObjectStats {
  val size: Vect2
  override def bounds(position: Vect2) = Bounds(position, size)
}

trait SizedWObject extends WObject with Mobility[Mobility.Static.type] {
  type Stats <: SizedWObjectStats
}
