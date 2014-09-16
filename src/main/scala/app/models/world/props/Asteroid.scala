package app.models.world.props

import app.models.world.{WObject, Vect2}

object Asteroid extends PropStats

case class Asteroid(
  id: WObject.Id, position: Vect2, resources: Int
) extends Prop {
  override def stats = Asteroid
  override type Self = this.type
  override type Stats = Asteroid.type
  override protected def self = this
}
