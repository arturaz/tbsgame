package app.models.game.world.buildings

import app.models.game.{Population, Actions, Owner}
import app.models.game.world._

object WarpGate extends BuildingCompanion[WarpGate] with SizedWObjectCompanion
with GivingActionsCompanion[WarpGate] with GivingPopulationCompanion[WarpGate] {
  override val maxHp = HP(650)
  override val size = Vect2(6, 3)
  override val isCritical: Boolean = true
  override val actionsGiven = Actions(3)
  override val populationGiven = Population(15)
  override val visibility = RectDistance(4)
  override val warpGiven = RectDistance(2)
  val kind = WObjKind.Heavy

  override def withNewHp(hp: HP)(self: WarpGate) = self.copy(hp = hp)
}

case class WarpGate(
  position: Vect2, owner: Owner,
  id: WObject.Id=WObject.newId, hp: HP=WarpGate.maxHp
) extends Building with GivingActions with GivingPopulation with SizedWObject {
  override def companion = WarpGate
  override type Self = WarpGate
  override type Companion = WarpGate.type
  override def self = this
}
