package app.models.game.world.buildings

import app.models.game.{Actions, Owner}
import app.models.game.world._

object WarpGate extends BuildingCompanion[WarpGate] with SizedWObjectCompanion
with GivingActionsCompanion[WarpGate] {
  override val maxHp = HP(35)
  override val size: Vect2 = Vect2(6, 4)
  override val isCritical: Boolean = true
  override val actionsGiven = Actions(4)
  override val defense = emptyRange
  override val visibility: Int = 3
  override val warpGiven = RectDistance(2)

  override def withNewHp(hp: HP)(self: WarpGate) = self.copy(hp = hp)
}

case class WarpGate(
  position: Vect2, owner: Owner,
  id: WObject.Id=WObject.newId, hp: HP=WarpGate.maxHp
) extends Building with GivingActions with SizedWObject {
  override def companion = WarpGate
  override type Self = WarpGate
  override type Companion = WarpGate.type
  override def self = this
}
