package app.models.game.world

import akka.event.LoggingAdapter
import app.models.game.Owner
import app.models.game.events.{Evented, HPChangeEvt}
import app.models.game.world.buildings._
import app.models.game.world.maps.WarpZoneMap
import app.models.game.world.units._
import implicits._
import app.models.game.world.Ops._
import scalaz._, Scalaz._

import scala.language.implicitConversions

trait OwnedObjStatsImpl { _: OwnedObjStats =>
  val maxHp: HP
  val maxDamagePerHit = Option.empty[HP]
  val visibility: RectDistance
  /* If team has no critical objects it cannot do any more actions and
     loses the game. */
  val isCritical = false
  val warpGiven = RectDistance(0)
  val kind: WObjKind
}

trait OwnedObjImpl extends WObjectImpl {
  type Stats <: OwnedObjStats
  val hp: HP
  val owner: Owner

  def maxHp = stats.maxHp
  def isWarpingIn = false
  def isWarpedIn = ! isWarpingIn
  def goingToWarpInNextTurn = false
  def isEnemy(o: OwnedObj) = owner.team =/= o.owner.team
  def isFriend(o: OwnedObj) = ! isEnemy(o)
  def destroyReward = Option.empty[Resources]

  def inWarpZone(world: World) = world.isValidForWarp(owner, position)

  lazy val visibility = stats.visibility.extend(bounds)
  def sees(obj: WObject) = visibility.intersects(obj.bounds)

  lazy val warpZone =
    if (stats.warpGiven.isNotZero) Some(stats.warpGiven.extend(bounds))
    else None
}

trait OwnedObjCompanion extends ToOwnedObjOps

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
  implicit def toOwnedObjOps[A <: OwnedObj](a: A): OwnedObjOps[A] = ((a: OwnedObj) match {
    /* Buildings */

    case o: Extractor => ExtractorOps(o)
    case o: LaserTower => LaserTowerOps(o)
    case o: Spawner => SpawnerOps(o)
    case o: VPTower => VPTowerOps(o)
    case o: WarpGate => WarpGateOps(o)
    case o: WarpLinker => WarpLinkerOps(o)
    case o: PopulationTower => PopulationTowerOps(o)
    case o: ActionTower => ActionTowerOps(o)

    /* Units */

    case o: Corvette => CorvetteOps(o)
    case o: Drone => DroneOps(o)
    case o: Fortress => FortressOps(o)
    case o: Gunship => GunshipOps(o)
    case o: WarpPrism => WarpPrismOps(o)
    case o: RayShip => RayShipOps(o)
    case o: RocketFrigate => RocketFrigateOps(o)
    case o: Scout => ScoutOps(o)
    case o: Wasp => WaspOps(o)
  }).asInstanceOf[OwnedObjOps[A]]
}
