package app.models.world.buildings

import app.models.Owner
import app.models.world._

object WarpGate extends BuildingCompanion[WarpGate]
with GivingActionsCompanion[WarpGate] {
  override val maxHp: Int = 35
  override val size: Vect2 = Vect2(6, 4)
  override val isCritical: Boolean = true
  override val actionsGiven: Int = 3

  override def withNewHp(hp: Int)(self: WarpGate) = self.copy(hp = hp)
}

case class WarpGate(
  position: Vect2, owner: Owner,
  id: WObject.Id=WObject.newId, hp: Int=WarpGate.maxHp
) extends Building with GivingActions {
  override def companion = WarpGate
  override type Self = WarpGate
  override type Companion = WarpGate.type
  override def self = this
}
