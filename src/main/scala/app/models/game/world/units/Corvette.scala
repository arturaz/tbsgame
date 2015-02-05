package app.models.game.world.units

import app.models.game.world._
import app.models.game.{Population, Actions, Player}

object Corvette extends WFighterUnitCompanion[Corvette]
with SpecialActionCompanion[Corvette]
{
  override val maxHp = HP(120)
  override val attack = Atk(70)
  override val attacks = Attacks(2)
  override val attackRange = RadialDistance.Four
  override val movement = Movement.fromTiles(12)
  override val visibility = RectDistance(4)
  override val warpTime = WarpTime(0)
  override val cost = Resources(6)
  override val populationCost = Population(2)
  override val kind = WObjKind.Medium

  val specialMovementAdded = Movement.fromTiles(6)
  override val specialActionsNeeded = Actions(0)

  override def warp(owner: Player, position: Vect2) = Corvette(position, owner)
  override def setWarpState(newState: WarpTime)(self: Corvette) =
    self.copy(warpState = newState)
  override def setMoveValues(position: Vect2, movementLeft: Movement)(self: Corvette) =
    self.copy(position = position, movementLeft = movementLeft)
  override def withNewHp(hp: HP)(self: Corvette) = self.copy(hp = hp)
  override def withNewXP(xp: XP)(self: Corvette) = self.copy(xp = xp)
  override def withAttacksLeft(value: Attacks)(self: Corvette) =
    self.copy(attacksLeft = value)
}

case class Corvette(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=Corvette.maxHp, xp: XP=Corvette.InitialXP,
  attacksLeft: Attacks=Corvette.InitialAttacks,
  movementLeft: Movement=Corvette.movement,
  warpState: WarpTime=Corvette.InitialWarpState
) extends WFighterUnit with SpecialAction {
  type Self = Corvette
  type Companion = Corvette.type
  override def self = this
  override def companion = Corvette

  override def specialImpl(world: World) = Either.cond(
    ! noAttacksLeft,
    {
      for {
        self <- companion.withAttacksLeftEvt(self.attacksLeft - Attacks(1))(world, self)
        self <- companion.setMoveValues(
          world, self,
          movementLeft + companion.specialMovementAdded
        )
        world <- world.updated(this, self)
      } yield world
    },
    s"Cannot toggle special action for $this: already attacked!"
  )
}
