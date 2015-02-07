package app.models.game.world

/* Objects which have size cannot be moved. */
trait SizedWObjectStats extends WObjectStats {
  val size: Vect2
  def bounds(position: Vect2) = Bounds(position, size)
}

trait SizedWObjectImpl extends WObjectImpl with Mobility[Mobility.Static.type] {
  val stats: SizedWObjectStats
}

