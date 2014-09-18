package app.models.world.buildings

import app.models.Owner
import app.models.world.{WObject, Vect2}

object WarpGate extends BuildingStats {
  override val maxHp: Int = 35
  override val size: Vect2 = Vect2(6, 4)
}

case class WarpGate(
  position: Vect2, owner: Owner,
  id: WObject.Id=WObject.newId, hp: Int=WarpGate.maxHp
) extends Building {
  override def stats = WarpGate
  override type Self = WarpGate
  override type Stats = WarpGate.type
  override protected def self = this
  override protected def withNewHp(hp: Int) = copy(hp = hp)
}
