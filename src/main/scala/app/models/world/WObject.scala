package app.models.world

trait WObjectStats {
  val size: Vect2
}

/* World object */
trait WObject {
  def position: Vect2
  def stats: WObjectStats
  lazy val bounds = Bounds(position, stats.size)

  /* Called when next turn is called. */
  def nextTurn(): Unit = ()
}
