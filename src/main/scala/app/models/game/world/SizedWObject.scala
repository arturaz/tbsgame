package app.models.game.world

trait SizedWObjectStatsImpl extends WObjectStatsImpl {
  override val size: Vect2
}

/* Objects which have size cannot be moved. */
trait SizedWObjectImpl extends WObjectImpl with MobilityStatic {
  type Stats <: SizedWObjectStats
}

