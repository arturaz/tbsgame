package app.models.world

/* Objects which have size cannot usually be moved. */
trait SizedWObjectStats extends WObjectStats {
  val size: Vect2
}

trait SizedWObject extends WObject {
  type Stats <: SizedWObjectStats

  override lazy val bounds = Bounds(position, stats.size)
}
