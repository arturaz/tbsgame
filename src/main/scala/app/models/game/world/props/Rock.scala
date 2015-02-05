package app.models.game.world.props

import app.models.game.world.{WObject, Vect2}

object Rock extends PropCompanion

/* Rock is an immovable 1x1 obstacle */
case class Rock(
  position: Vect2, id: WObject.Id=WObject.newId
) extends Prop {
  override def companion = Rock
  override type Self = this.type
  override type Companion = Rock.type
  override def self = this
}
