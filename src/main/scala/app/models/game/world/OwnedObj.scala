package app.models.game.world

import akka.event.LoggingAdapter
import app.models.game.Owner
import app.models.game.events.{Evented, HPChangeEvt}
import app.models.game.world.buildings.Extractor
import implicits._

import scala.language.implicitConversions

trait OwnedObjStats extends WObjectStats {
  val maxHp: HP
  val visibility: RectDistance
  /* If team has no critical objects it cannot do any more actions and
     loses the game. */
  val isCritical: Boolean = false
  val warpGiven: RectDistance = RectDistance(0)
  val kind: WObjKind
}

trait OwnedObjImpl extends WObjectImpl {
  val stats: OwnedObjStats
  val hp: HP
  val owner: Owner

  def maxHp = stats.maxHp
  def isWarpingIn = false
  def isWarpedIn = ! isWarpingIn
  def isEnemy(o: OwnedObj) = owner.team =/= o.owner.team
  def isFriend(o: OwnedObj) = ! isEnemy(o)
  def destroyReward = Option.empty[Resources]

  lazy val visibility = stats.visibility.extend(bounds)
  def sees(obj: WObject) = visibility.intersects(obj.bounds)

  lazy val warpZone =
    if (stats.warpGiven.isNotZero) Some(stats.warpGiven.extend(bounds))
    else None
}

object OwnedObj extends ToOwnedObjOps {
  def teamTurnStarted
  (obj: OwnedObj, world: World)(implicit log: LoggingAdapter)
  : Evented[(World, OwnedObj)] = {
    import GivingVictoryPoints.toGivingVictoryPointsOps

    Evented((world, obj)) |>
      WObject.ifIs[GivingVictoryPoints].raw((w, o) => (o.teamTurnStarted(w), o))
  }

  def teamTurnFinished
  (obj: OwnedObj, world: World)(implicit log: LoggingAdapter)
  : Evented[(World, OwnedObj)] = {
    import app.models.game.world.Movable.toMovableOps

    Evented((world, obj)) |>
      WObject.ifIs[Movable].evt((w, o) => o.teamTurnFinishedSelf(w))
  }
}

trait OwnedObjOps[Self <: OwnedObj] {
  def self: Self
  def withNewHp(hp: HP): Self
  def withNewHPEvt(hp: HP)(world: World): Evented[Self] = {
    val newSelf = withNewHp(hp)
    Evented(
      newSelf,
      if (self.hp == newSelf.hp) Vector.empty else Vector(HPChangeEvt(world, newSelf))
    )
  }

  def takeDamage(damage: HP): Option[Self] =
    if (self.hp <= damage) None else Some(withNewHp(self.hp - damage))
}

trait ToOwnedObjOps {
  implicit def toOwnedObjOps[A <: OwnedObj](a: A): OwnedObjOps[A] = (a match {
    case o: Extractor => Extractor.Ops(o)
  }).asInstanceOf[OwnedObjOps[A]]
}
