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
  val warpGiven: RectDistance = RectDistance(0)

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
  override def asOwnedObj = Some(this)
  def defense = if (isWarpingIn) 0 to 0 else companion.defense
  def destroyReward = Option.empty[Resources]

  lazy val visibility = RectDistance(companion.visibility).extend(bounds)
  def sees(obj: WObject) = visibility.intersects(obj.bounds)

  lazy val warpZone =
    if (companion.warpGiven.isNotZero) Some(companion.warpGiven.extend(bounds))
    else None

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
    World.revealObjects(self.owner.team, evt.map(_._1)).map((_, evt.value._2))
  }
}

trait PlayerObj extends OwnedObj { val owner: Player }
trait HumanObj extends PlayerObj { val owner: Human }
trait BotObj extends PlayerObj { val owner: Bot }
