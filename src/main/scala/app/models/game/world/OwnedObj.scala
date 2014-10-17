package app.models.game.world

import app.models.game.{Bot, Human, Player, Owner}
import app.models.game.events.Evented
import implicits._

trait OwnedObjOps[Self] extends WObjectOps {
  def withNewHp(hp: HP)(self: Self): Self
}

trait OwnedObjStats extends WObjectStats {
  val maxHp: HP
  val visibility: Int
  val defense: Range.Inclusive
  /* If team has no critical objects it cannot do any more actions and
     loses the game. */
  val isCritical: Boolean = false

  protected def emptyRange = 0 to -1
}

trait OwnedObjCompanion[Self]
extends OwnedObjOps[Self] with OwnedObjStats

/* Object that belongs to some faction and not just a world prop */
trait OwnedObj extends WObject {
  type Self <: OwnedObj
  type Companion <: OwnedObjOps[Self] with OwnedObjStats

  val hp: HP
  val owner: Owner
  def isWarpingIn = false
  def isWarpedIn = ! isWarpingIn
  def isEnemy(o: OwnedObj) = owner.team =/= o.owner.team
  def isFriend(o: OwnedObj) = ! isEnemy(o)
  def givesWarpVisibility: Boolean
  override def asOwnedObj = Some(this)

  lazy val visibility = {
    val vis = companion.visibility
    Bounds(
      bounds.x.start - vis to bounds.x.end + vis,
      bounds.y.start - vis to bounds.y.end + vis
    )
  }
  def sees(obj: WObject) = visibility.intersects(obj.bounds)

  def takeDamage: Option[Self] =
    if (hp === HP(1)) None else Some(self |> companion.withNewHp(hp - HP(1)))

  def teamTurnStartedSelf(world: World): WorldSelfUpdate = Evented((world, self))
  final def teamTurnStarted(world: World): Evented[World] =
    teamTurnStartedSelf(world).map(_._1)

  def teamTurnFinishedSelf(world: World): WorldSelfUpdate = Evented((world, self))
  final def teamTurnFinished(world: World): Evented[World] =
    teamTurnFinishedSelf(world).map(_._1)

  override protected def selfEventedUpdate(
    f: (World, Self) => Evented[Self]
  )(evented: WorldSelfUpdate) = evented |> super.selfEventedUpdate(f) |> { evt =>
    evt.flatMap { case (world, self) =>
      World.revealObjects(self.owner.team, Evented(world, evt.events)).map((_, self))
    }
  }
}

trait PlayerObj extends OwnedObj { val owner: Player }
trait HumanObj extends PlayerObj { val owner: Human }
trait BotObj extends PlayerObj { val owner: Bot }