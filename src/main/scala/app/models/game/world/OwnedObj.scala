package app.models.game.world

import akka.event.LoggingAdapter
import app.models.game._
import app.models.game.events.{HPChangeEvt, Evented}
import implicits._

trait OwnedObjOps[Self <: OwnedObj] extends WObjectOps {
  def withNewHp(hp: HP)(self: Self): Self
  def withNewHPEvt(hp: HP)(world: World, self: Self): Evented[Self] = {
    val newSelf = self |> withNewHp(hp)
    Evented(
      newSelf,
      if (self.hp == newSelf.hp) Vector.empty else Vector(HPChangeEvt(world, newSelf))
    )
  }
}

trait OwnedObjStats extends WObjectStats {
  val maxHp: HP
  val visibility: RectDistance
  /* If team has no critical objects it cannot do any more actions and
     loses the game. */
  val isCritical: Boolean = false
  val warpGiven: RectDistance = RectDistance(0)
  val kind: WObjKind
}

trait OwnedObjCompanion[Self <: OwnedObj]
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
  def destroyReward = Option.empty[Resources]

  lazy val visibility = companion.visibility.extend(bounds)
  def sees(obj: WObject) = visibility.intersects(obj.bounds)

  lazy val warpZone =
    if (companion.warpGiven.isNotZero) Some(companion.warpGiven.extend(bounds))
    else None

  def takeDamage(damage: HP): Option[Self] =
    if (hp <= damage) None else Some(self |> companion.withNewHp(hp - damage))

  def teamTurnStartedSelf(world: World)(implicit log: LoggingAdapter): WorldSelfUpdate =
    Evented((world, self))
  final def teamTurnStarted(world: World)(implicit log: LoggingAdapter): Evented[World] =
    teamTurnStartedSelf(world).map(_._1)

  def teamTurnFinishedSelf(world: World)(implicit log: LoggingAdapter): WorldSelfUpdate =
    Evented((world, self))
  final def teamTurnFinished(world: World)(implicit log: LoggingAdapter): Evented[World] =
    teamTurnFinishedSelf(world).map(_._1)

  override protected def selfEventedUpdate(
    f: (World, Self) => Evented[Self]
  )(evented: WorldSelfUpdate) = evented |> super.selfEventedUpdate(f) |> { evt =>
    World.revealObjects(self.owner.team, evt.map(_._1)).map((_, evt.value._2))
  }
}

trait TeamObj extends OwnedObj { val owner: Team }
trait PlayerObj extends OwnedObj { val owner: Player }
trait HumanObj extends PlayerObj { val owner: Human }
trait BotObj extends PlayerObj { val owner: Bot }
