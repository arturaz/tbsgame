package app.models.game.world.units

import app.models.game.Player
import app.models.game.world._

/**
 * Created by arturas on 2014-11-03.
 */
object Scout extends WUnitCompanion[Scout] {
  override val maxHp = HP(25)
  override val warpTime = WarpTime(0)
  override val cost = Resources(5)
  override val movement = TileDistance(16)
  override val defense = emptyRange
  override val visibility = RectDistance(7)
  override val kind = WObjKind.Light

  override protected def withMovedOrAttacked(value: Boolean)(self: Scout) =
    self.copy(movedOrAttacked = value)
  override protected def setMoveValues(
    position: Vect2, movementLeft: TileDistance
  )(self: Scout) = self.copy(position = position, movementLeft = movementLeft)
  override def setWarpState(newState: WarpTime)(self: Scout) =
    self.copy(warpState = newState)
  override def withNewHp(hp: HP)(self: Scout) = self.copy(hp = hp)
  override def warp(owner: Player, position: Vect2) = Scout(position, owner)
}

case class Scout(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=Scout.maxHp,
  movementLeft: TileDistance=Scout.movement,
  warpState: WarpTime=Scout.InitialWarpState,
  movedOrAttacked: Boolean=Scout.InitialMovedOrAttacked
) extends WUnit {
  type Self = Scout
  type Companion = Scout.type
  def companion = Scout
  def self = this
}