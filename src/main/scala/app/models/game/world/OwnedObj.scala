package app.models.game.world

import akka.event.LoggingAdapter
import app.models.game.Owner
import app.models.game.events.{Evented, HPChangeEvt}
import app.models.game.world.buildings._
import app.models.game.world.units._
import implicits._
import app.models.game.world.Ops._

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
  type Stats <: OwnedObjStats
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

trait OwnedObjCompanion extends ToOwnedObjOps {
  def teamTurnStarted
  (obj: OwnedObj, world: World)(implicit log: LoggingAdapter)
  : Evented[(World, Option[OwnedObj])] = {
    Evented((world, obj)) |>
      WObject.ifIs[Warpable].evt((w, o) => o.warpableTeamTurnStarted(w)) |>
      WObject.ifIs[GivingVictoryPoints].rawWorld((w, o) => o.givingVPsTeamTurnStarted(w)) |>
      WObject.ifIs[Extractor].evtWorld((w, o) => o.extractorTeamTurnStarted(w)) |>
      WObject.ifIs[Movable].evt((w, o) => o.movableTeamTurnStarted(w)) |>
      WObject.ifIs[Fighter].evt((w, o) => o.fighterTeamTurnStarted(w)) |>
      (_.map { case (newWorld, newObj) => (newWorld, Some(newObj)) })
  }

  def teamTurnFinished
  (obj: OwnedObj, world: World)(implicit log: LoggingAdapter)
  : Evented[(World, Option[OwnedObj])] = {
    Evented((world, obj)) |>
      WObject.ifIs[ReactiveFighter].evtOpt((w, o) => o.reactiveFighterTeamTurnFinished(w))
  }
}

trait OwnedObjOps[+Self <: OwnedObj] {
  def self: Self

  protected def withNewHp(hp: HP): Self
  def withNewHPEvt(hp: HP)(world: World): Evented[Self] = {
    val newSelf = withNewHp(hp)
    Evented(
      newSelf,
      if (self.hp == newSelf.hp) Vector.empty
      else Vector(HPChangeEvt(world.visibilityMap, newSelf))
    )
  }

  def takeDamage(damage: HP): Option[Self] =
    if (self.hp <= damage) None else Some(withNewHp(self.hp - damage))
}

trait ToOwnedObjOps {
  implicit def toOwnedObjOps[A <: OwnedObj](a: A): OwnedObjOps[A] = (a match {
    /* Buildings */

    case o: Extractor => ExtractorOps(o)
    case o: LaserTower => LaserTowerOps(o)
    case o: Spawner => SpawnerOps(o)
    case o: VPTower => VPTowerOps(o)
    case o: WarpGate => WarpGateOps(o)
    case o: WarpLinker => WarpLinkerOps(o)

    /* Units */

    case o: Corvette => CorvetteOps(o)
    case o: Fortress => FortressOps(o)
    case o: Gunship => GunshipOps(o)
    case o: RayShip => RayShipOps(o)
    case o: RocketFrigate => RocketFrigateOps(o)
    case o: Scout => ScoutOps(o)
    case o: Wasp => WaspOps(o)
  }).asInstanceOf[OwnedObjOps[A]]
}
