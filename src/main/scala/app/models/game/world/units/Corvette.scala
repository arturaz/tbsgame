package app.models.game.world.units

import app.models.game.world._
import app.models.game.{Actions, Player}

object Corvette extends WUnitCompanion[Corvette] 
with FighterCompanion[Corvette] with SpecialActionCompanion[Corvette] {
  override val attack = 4 to 6
  override val defense = 0 to 1
  override val attackRange = TileDistance(5)
  override val movement = TileDistance(6)
  override val visibility = RectDistance(4)
  override val maxHp = HP(10)
  override val warpTime = WarpTime(0)
  override val cost = Resources(6)
  override val kind = WObjKind.Medium
  override val attacks = Attacks(1)

  val specialMovementAdded = TileDistance(7)
  override val specialActionsNeeded = Actions(0)

  override def warp(owner: Player, position: Vect2) = Corvette(position, owner)
  override def setWarpState(newState: WarpTime)(self: Corvette) =
    self.copy(warpState = newState)
  override def setMoveValues(position: Vect2, movementLeft: TileDistance)(self: Corvette) =
    self.copy(position = position, movementLeft = movementLeft)
  override def withMovedOrAttacked(value: Boolean)(self: Corvette) =
    self.copy(movedOrAttacked = value)
  override def withNewHp(hp: HP)(self: Corvette) =
    self.copy(hp = hp)
  override def withAttacksLeft(value: Attacks)(self: Corvette) =
    self.copy(attacksLeft = value)
}

case class Corvette(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=Corvette.maxHp,
  attacksLeft: Attacks=Corvette.InitialAttacks,
  movementLeft: TileDistance=Corvette.movement,
  warpState: WarpTime=Corvette.InitialWarpState,
  movedOrAttacked: Boolean=Corvette.InitialMovedOrAttacked
) extends WUnit with Fighter with SpecialAction {
  type Self = Corvette
  type Companion = Corvette.type
  override def self = this
  override def companion = Corvette

  override def specialImpl(world: World) = Either.cond(
    ! noAttacksLeft,
    {
      val multiplier = TileDistance(self.attacksLeft.value)
      for {
        self <- companion.withAttacksLeftEvt(Attacks(0))(world, self)
        self <- companion.setMoveValues(
          world, self,
          movementLeft + companion.specialMovementAdded * multiplier
        )
        world <- world.updated(this, self)
      } yield world
    },
    s"Cannot toggle special action for $this: already attacked!"
  )
}
