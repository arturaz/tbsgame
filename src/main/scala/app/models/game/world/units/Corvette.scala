package app.models.game.world.units

import app.models.game.Player
import app.models.game.events.MovementChangeEvt
import app.models.game.world._

object Corvette extends WUnitCompanion[Corvette] 
with FighterCompanion[Corvette] with SpecialActionCompanion {
  override val attack = 2 to 7
  override val defense = 2 to 7
  override val attackRange = TileDistance(3)
  override val movement = TileDistance(3)
  override val visibility: Int = 4
  override val maxHp: Int = 1
  override val warpTime: Int = 0
  override val cost: Int = 5

  val specialMovementAdded = TileDistance(4)
  override val specialActionsNeeded: Int = 0

  override def warp(owner: Player, position: Vect2) = Corvette(position, owner)
  override def setWarpState(newState: Int)(self: Corvette) =
    self.copy(warpState = newState)
  override def setMoveValues(position: Vect2, movementLeft: TileDistance)(self: Corvette) =
    self.copy(position = position, movementLeft = movementLeft)
  override def withMovedOrAttacked(value: Boolean)(self: Corvette) =
    self.copy(movedOrAttacked = value)
  override def withNewHp(hp: Int)(self: Corvette) =
    self.copy(hp = hp)
  override def attacked(value: Boolean)(self: Corvette) =
    self.copy(hasAttacked = value)
}

case class Corvette(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: Int=Wasp.maxHp, hasAttacked: Boolean=false,
  movementLeft: TileDistance=Corvette.movement, warpState: Int=Corvette.InitialWarpState,
  movedOrAttacked: Boolean=Corvette.InitialMovedOrAttacked
) extends WUnit with Fighter with SpecialAction {
  type Self = Corvette
  type Companion = Corvette.type
  override def self = this
  override def companion = Corvette

  override def special(world: World) = Either.cond(
    ! hasAttacked,
    {
      val updated = copy(
        hasAttacked = true,
        movementLeft = movementLeft + companion.specialMovementAdded
      )
      world.updated(this, updated) :+ MovementChangeEvt(world, updated)
    },
    s"Cannot toggle special action for $this: already attacked!"
  )
}
