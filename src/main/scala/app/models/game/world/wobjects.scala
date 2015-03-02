package app.models.game.world

import java.util.UUID

import app.models.game._
import app.models.game.world.buildings._
import app.models.game.world.props.PropImpl
import app.models.game.world.units._
import utils.IdObj

/********************************* [ PROPS ] *********************************/

/* Asteroids can be mined for resources */
case class Asteroid(
  position: Vect2, resources: Resources, extractionSpeed: Resources,
  id: WObject.Id=WObject.newId
) extends Prop {
  type Stats = AsteroidStats.type
  override val stats = AsteroidStats
}
object AsteroidStats extends WObjectStats

/* Rock is an immovable 1x1 obstacle */
case class Rock(
  position: Vect2, id: WObject.Id=WObject.newId
) extends Prop {
  type Stats = RockStats.type
  override val stats = RockStats
}
object RockStats extends WObjectStats {
  override val blocksVisibility = true
  override val blocksWarp = true
}

/********************************* [ BUILDINGS ] *********************************/

/* Main building of a player */
case class WarpGate(
  position: Vect2, owner: Team,
  hp: HP=WarpGateStats.maxHp, id: WObject.Id=WObject.newId
) extends WarpGateImpl with TeamBuilding with GivingActions with GivingPopulation
with SizedWObject with SpecialAction with SpecialAtEndOfTurn {
  type Stats = WarpGateStats.type
  override val stats = WarpGateStats
  override def endOfTurnPriority = 0
}

/* Gives victory points each turn to its owner */
case class VPTower(
  position: Vect2, owner: Team,
  hp: HP=VPTowerStats.maxHp, id: WObject.Id=WObject.newId
) extends VPTowerImpl with TeamBuilding with GivingActions with RespawnsOnDestruction
with SizedWObject with GivingVictoryPoints {
  type Stats = VPTowerStats.type
  override val stats = VPTowerStats
}

/* Extracts resources from asteroids */
case class Extractor(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=ExtractorStats.maxHp,
  warpState: WarpTime=ExtractorStats.InitialWarpState
) extends ExtractorImpl with PlayerBuilding with Building with Warpable
with SpecialAction {
  type Stats = ExtractorStats.type
  override val stats = ExtractorStats
}

/* Extends warp zone for players */
case class WarpLinker(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=WarpLinkerStats.maxHp,
  warpState: WarpTime=WarpLinkerStats.InitialWarpState
) extends PlayerBuilding with Building with Warpable {
  type Stats = WarpLinkerStats.type
  override val stats = WarpLinkerStats
}

/* Defensive tower */
case class LaserTower(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=LaserTowerStats.maxHp,
  xp: XP=LaserTowerStats.InitialXP,
  warpState: WarpTime=LaserTowerStats.InitialWarpState,
  attacksLeft: Attacks=LaserTowerStats.InitialAttacks
) extends LaserTowerImpl with PlayerBuilding with Building with Warpable
with ReactiveFighter with SpecialAction {
  type Stats = LaserTowerStats.type
  override val stats = LaserTowerStats
}

/* Enemy base */
case class Spawner(
  position: Vect2, owner: Bot,
  id: WObject.Id=WObject.newId,
  startingStrength: SpawnerStr=SpawnerStats.DefaultStartingStrength,
  turnsPerStrength: SpawnerStr=SpawnerStats.DefaultTurnsPerStrength,
  turns: Int=0, hp: HP=SpawnerStats.maxHp
) extends SpawnerImpl with BotBuilding with TurnCounter with SizedWObject

/********************************* [ UNITS ] *********************************/

case class Scout(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=ScoutStats.maxHp,
  movementLeft: Movement=ScoutStats.InitialMovement,
  warpState: WarpTime=ScoutStats.InitialWarpState
) extends WUnit {
  type Stats = ScoutStats.type
  override val stats = ScoutStats
}

case class Corvette(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=CorvetteStats.maxHp,
  xp: XP=CorvetteStats.InitialXP,
  attacksLeft: Attacks=CorvetteStats.InitialAttacks,
  movementLeft: Movement=CorvetteStats.InitialMovement,
  warpState: WarpTime=CorvetteStats.InitialWarpState
) extends CorvetteImpl with WUnit with Fighter with SpecialAction

case class Gunship(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=GunshipStats.maxHp, xp: XP=GunshipStats.InitialXP,
  attacksLeft: Attacks=GunshipStats.InitialAttacks,
  movementLeft: Movement=GunshipStats.InitialMovement,
  warpState: WarpTime=GunshipStats.InitialWarpState
) extends WUnit with Fighter {
  type Stats = GunshipStats.type
  override val stats = GunshipStats
}

case class RocketFrigate(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=RocketFrigateStats.maxHp,
  xp: XP=RocketFrigateStats.InitialXP,
  attacksLeft: Attacks=RocketFrigateStats.InitialAttacks,
  movementLeft: Movement=RocketFrigateStats.InitialMovement,
  warpState: WarpTime=RocketFrigateStats.InitialWarpState
) extends WUnit with Fighter {
  type Stats = RocketFrigateStats.type
  override val stats = RocketFrigateStats
}

case class Wasp(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=WaspStats.maxHp, xp: XP=WaspStats.InitialXP,
  attacksLeft: Attacks=WaspStats.InitialAttacks,
  movementLeft: Movement=WaspStats.InitialMovement,
  warpState: WarpTime=WaspStats.InitialWarpState
) extends WUnit with Fighter {
  type Stats = WaspStats.type
  override val stats = WaspStats
}

case class RayShip(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=RayShipStats.maxHp, xp: XP=RayShipStats.InitialXP,
  attacksLeft: Attacks=RayShipStats.InitialAttacks,
  movementLeft: Movement=RayShipStats.InitialMovement,
  warpState: WarpTime=RayShipStats.InitialWarpState
) extends WUnit with Fighter {
  type Stats = RayShipStats.type
  override val stats = RayShipStats
}

case class Fortress(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=FortressStats.maxHp, xp: XP=FortressStats.InitialXP,
  attacksLeft: Attacks=FortressStats.InitialAttacks,
  movementLeft: Movement=FortressStats.InitialMovement,
  warpState: WarpTime=FortressStats.InitialWarpState
) extends WUnit with Fighter {
  type Stats = FortressStats.type
  override val stats = FortressStats
}

/********************************* [ TRAITS ] *********************************/

/* World object */
sealed trait WObject extends WObjectImpl
object WObject extends WObjectCompanion {
  case class Id(id: UUID) extends AnyVal with IdObj {
    override protected def prefix = "WObjID"
  }
}

/* Because WObject is sealed we cannot extend it in tests :| */
trait WObjectTestRoot extends WObject

/* A property in the world */
sealed trait Prop extends WObject with PropImpl

/* Object that belongs to some faction and not just a world prop */
sealed trait OwnedObj extends WObject with OwnedObjImpl
object OwnedObj extends OwnedObjCompanion

sealed trait TeamObj extends OwnedObj { val owner: Team }
sealed trait PlayerObj extends OwnedObj { val owner: Player }
sealed trait HumanObj extends PlayerObj { val owner: Human }
sealed trait BotObj extends PlayerObj { val owner: Bot }

/* Gives actions to its owner */
sealed trait GivingActions extends OwnedObj with GivingActionsImpl

/* Increases population cap to its owner. */
sealed trait GivingPopulation extends OwnedObj with GivingPopulationImpl

/* Gives victory points each turn */
sealed trait GivingVictoryPoints extends OwnedObj with GivingVictoryPointsImpl

/* Objects that can move. All such objects have 1x1 size. */
sealed trait Movable extends OwnedObj with MovableImpl

/* Objects that can do a special action. */
sealed trait SpecialAction extends OwnedObj with SpecialActionImpl

/* Objects that are bigger 1x1. Such objects cannot be moved. */
sealed trait SizedWObject extends WObject with SizedWObjectImpl

/* Has an internal counter that increases at each game turn. */
sealed trait TurnCounter extends WObject with TurnCounterImpl

/* Can attack. */
sealed trait Fighter extends OwnedObj with FighterImpl

/* Can attack reactively not on owners turn. */
sealed trait ReactiveFighter extends Fighter with ReactiveFighterImpl

/* If destroyed this object should respawn with new owner and hp. */
sealed trait RespawnsOnDestruction extends OwnedObj with RespawnsOnDestructionImpl

/* Marker that says use special action at end of turn for all the possible actions. */
sealed trait SpecialAtEndOfTurn extends SpecialAction {
  /* Lowest is first */
  def endOfTurnPriority: Int
}

/* Things that can be warped by in a player/bot. */
sealed trait Warpable extends OwnedObj with WarpableImpl
object Warpable extends ToWarpableOps {
  /* Actions needed to warp in something */
  val ActionsNeeded = Actions(1)
}

/* Buildings are stationary */
sealed trait Building extends OwnedObj with BuildingImpl
sealed trait TeamBuilding extends Building with TeamObj
sealed trait PlayerBuilding extends Building with PlayerObj
sealed trait HumanBuilding extends Building with HumanObj
sealed trait BotBuilding extends Building with BotObj

/* World unit - player controlled movable & warpable objects. */
sealed trait WUnit extends WUnitImpl with PlayerObj with Movable with Warpable