package app.models.game.world.units

import app.models.game.Player
import app.models.game.world._

/**
 * Created by arturas on 2014-11-04.
 */
object RayShip extends WUnitCompanion[RayShip] with FighterCompanion[RayShip] {
  override val maxHp = HP(4)
  override val attackRange = TileDistance(7)
  override val attack = 3 to 7
  override val defense = 0 to 1
  override val visibility = 6
  override val cost = Resources(10)
  override val warpTime = WarpTime(0)
  override val movement = TileDistance(2)
  override val kind = WObjKind.Light
  override val attacks = Attacks(1)

  override def warp(owner: Player, position: Vect2) = RayShip(position, owner)
  override protected def withAttacksLeft(value: Attacks)(self: RayShip) =
    self.copy(attacksLeft = value)
  override protected def withMovedOrAttacked(value: Boolean)(self: RayShip) =
    self.copy(movedOrAttacked = value)
  override def withNewHp(hp: HP)(self: RayShip) = self.copy(hp = hp)
  override def setWarpState(newState: WarpTime)(self: RayShip) =
    self.copy(warpState = newState)
  override protected def setMoveValues(
    position: Vect2, movementLeft: TileDistance
  )(self: RayShip) = self.copy(position = position, movementLeft = movementLeft)
}

case class RayShip(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=RayShip.maxHp,
  attacksLeft: Attacks=RayShip.InitialAttacks,
  movementLeft: TileDistance=RayShip.movement, warpState: WarpTime=RayShip.InitialWarpState,
  movedOrAttacked: Boolean=RayShip.InitialMovedOrAttacked
) extends WUnit with Fighter {
  type Self = RayShip
  type Companion = RayShip.type
  def self = this
  def companion = RayShip
}