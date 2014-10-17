package app.models.game.world

import app.models.game.events.{WarpEvt, Evented}
import implicits._

import app.models.game.{Actions, Player}

trait WarpableOps[Self <: Warpable] extends OwnedObjOps[Self] {
  /* Create Warpable at given position. */
  protected def warpWOReactionImpl(
    world: World, owner: Player, position: Vect2
  ): Either[String, Self]

  private[this] def warpWOReaction(
    world: World, owner: Player, position: Vect2
  ): Either[String, Self] = {
    val b = bounds(position)
    if (world.isVisibleFull(owner, b)) warpWOReactionImpl(world, owner, position)
    else s"$b is not fully visible for $owner".left
  }

  /* Warp with reactions applied. */
  def warp(
    world: World, player: Player, position: Vect2
  ): Either[String, WObject.WorldObjOptUpdate[Self]] =
    warpWOReaction(world, player, position).right.map { warpedIn =>
      (Vector(WarpEvt(world, warpedIn)) ++: world.add(warpedIn)).
        flatMap(_.reactTo(warpedIn))
    }

  def warpW(
    world: World, player: Player, position: Vect2
  ): Either[String, Evented[World]] =
    warp(world, player, position).right.map { _.map(_._1) }

  def setWarpState(newState: WarpTime)(self: Self): Self
  def nextWarpState(world: World, self: Self): Evented[Self] =
    if (self.isWarpedIn) Evented(self)
    else {
      val newSelf = self |> setWarpState(self.warpState + WarpTime(1))
      Evented(newSelf, Vector(WarpEvt(world, newSelf)))
    }
}

trait WarpableStats extends OwnedObjStats {
  val InitialWarpState = WarpTime(0)
  val warpTime: WarpTime
  val cost: Resources
}

trait WarpableCompanion[Self <: Warpable] extends WarpableOps[Self]
with WarpableStats

object Warpable {
  /* Actions needed to warp in something */
  val ActionsNeeded = Actions(1)
}

trait Warpable extends OwnedObj {
  type Self <: Warpable
  type Companion <: WarpableOps[Self] with WarpableStats

  val warpState: WarpTime
  override def isWarpingIn = warpState < companion.warpTime

  override def teamTurnStartedSelf(world: World) =
    super.teamTurnStartedSelf(world) |>
    selfEventedUpdate(companion.nextWarpState)
}

trait EmptySpaceWarpableOps[Self <: Warpable] { _: WarpableOps[Self] =>
  def warp(owner: Player, position: Vect2): Self

  override def warpWOReactionImpl(
    world: World, owner: Player, position: Vect2
  ): Either[String, Self] = {
    val b = bounds(position)
    if (! world.canWarp(b)) s"Can't warp in because $b is taken in $world".left
    else warp(owner, position).right
  }
}

trait EmptySpaceWarpableCompanion[Self <: Warpable]
extends WarpableCompanion[Self] with EmptySpaceWarpableOps[Self]