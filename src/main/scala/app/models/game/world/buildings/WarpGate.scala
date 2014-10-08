package app.models.game.world.buildings

import app.models.game.Owner
import app.models.game.world._

object WarpGate extends BuildingCompanion[WarpGate]
with GivingActionsCompanion[WarpGate] {
  override val maxHp: Int = 35
  override val size: Vect2 = Vect2(6, 4)
  override val isCritical: Boolean = true
  override val actionsGiven: Int = 3
  override val defense = emptyRange

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
