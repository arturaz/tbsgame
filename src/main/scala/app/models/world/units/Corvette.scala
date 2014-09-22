package app.models.world.units

import app.models.Player
import app.models.game.events.MovementChangeEvt
import app.models.world._

object Corvette extends WUnitStats[Corvette] with FighterStats with SpecialActionStats {
  override val attack: Range = 2 to 7
  override val defense: Range = 2 to 7
  override val attackRange = TileDistance(3)
  override val movement = TileDistance(3)
  override val visibility: Int = 4
  override val maxHp: Int = 1
  override val warpTime: Int = 0
  override val cost: Int = 5

  val specialMovementAdded = TileDistance(4)
  override val specialActionsNeeded: Int = 0

  override def warp(owner: Player, position: Vect2) = Corvette(position, owner)
}

case class Corvette(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: Int=Wasp.maxHp, hasAttacked: Boolean=false,
  movementLeft: TileDistance=Corvette.movement, warpState: Int=Corvette.InitialWarpState,
  movedOrAttacked: Boolean=Corvette.InitialMovedOrAttacked
) extends WUnit with Fighter with SpecialAction {
  type Self = Corvette
  type Stats = Corvette.type
  override protected def self = this

  override def stats = Corvette

  override protected def setMoveValues(
    target: Vect2, movementLeft: TileDistance
  )(self: Self) =
    self.copy(position = target, movementLeft = movementLeft)
  override protected def advanceWarpState(self: Self, newState: Int) =
    self.copy(warpState = newState)
  override protected def attacked(value: Boolean)(self: Self) =
    self.copy(hasAttacked = value)
  override protected def withNewHp(hp: Int) = copy(hp = hp)
  override protected def withMovedOrAttacked(value: Boolean)(self: Self) =
    self.copy(movedOrAttacked = value)

  override def special(world: World) = Either.cond(
    ! hasAttacked,
    {
      val updated = copy(
        hasAttacked = true,
        movementLeft = movementLeft + stats.specialMovementAdded
      )
      (
        world.update(this, updated),
        Vector(MovementChangeEvt(updated.id, updated.movementLeft))
      )
    },
    s"Cannot toggle special action for $this: already attacked!"
  )
}
