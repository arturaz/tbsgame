package app.models.world.props

import app.models.world.{WObject, Vect2}
import monocle.Lenser

object Asteroid extends PropCompanion {
  private[this] val lenser = Lenser[Asteroid]
  val position = lenser(_.position)
  val resources = lenser(_.resources)
}

case class Asteroid(
  position: Vect2, resources: Int,
  id: WObject.Id=WObject.newId
) extends Prop {
  override def companion = Asteroid
  override type Self = this.type
  override type Companion = Asteroid.type
  override protected def self = this
}
