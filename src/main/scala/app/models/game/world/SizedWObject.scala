package app.models.game.world

trait SizedWObjectStats extends WObjectStats {
  override val size: Vect2
}

/* Objects which have size cannot be moved. */
trait SizedWObjectImpl extends WObjectImpl with Mobility[Mobility.Static.type] {
  type Stats <: SizedWObjectStats
}

