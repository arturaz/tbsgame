package app.models.world.props

import app.models.world.Vect2

object Asteroid extends PropStats {
  override val size = Vect2(1, 1)
}

class Asteroid(val position: Vect2, var resources: Int) extends Prop {
  override def stats = Asteroid
}
