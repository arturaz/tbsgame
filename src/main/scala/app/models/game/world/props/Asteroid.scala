package app.models.game.world.props

import app.models.game.world.{Resources, WObject, Vect2}
import monocle.Lenser

object Asteroid extends PropCompanion {
  private[this] val lenser = Lenser[Asteroid]
  val position = lenser(_.position)
  val resources = lenser(_.resources)
}

case class Asteroid(
  position: Vect2, resources: Resources,
  id: WObject.Id=WObject.newId
) extends Prop {
  override def companion = Asteroid
  override type Self = this.type
  override type Companion = Asteroid.type
  override def self = this
}
