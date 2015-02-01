package app.models.game.world.units

import app.models.game.{Population, Player}
import app.models.game.world._

/**
 * Created by arturas on 2014-11-03.
 */
object Scout extends WUnitCompanion[Scout] {
  override val maxHp = HP(70)
  override val warpTime = WarpTime(0)
  override val cost = Resources(5)
  override val populationCost = Population(1)
  override val movement = Movement.fromTiles(25)
  override val visibility = RectDistance(8)
  override val kind = WObjKind.Light

  override protected def setMoveValues(
    position: Vect2, movementLeft: Movement
  )(self: Scout) = self.copy(position = position, movementLeft = movementLeft)
  override def setWarpState(newState: WarpTime)(self: Scout) =
    self.copy(warpState = newState)
  override def withNewHp(hp: HP)(self: Scout) = self.copy(hp = hp)
  override def warp(owner: Player, position: Vect2) = Scout(position, owner)
}

case class Scout(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=Scout.maxHp,
  movementLeft: Movement=Scout.movement,
  warpState: WarpTime=Scout.InitialWarpState
) extends WUnit {
  type Self = Scout
  type Companion = Scout.type
  def companion = Scout
  def self = this
}