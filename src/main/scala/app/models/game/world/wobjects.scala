package app.models.game.world

import java.util.UUID

import app.models.game._
import app.models.game.world.buildings._
import app.models.game.world.props.{ExtractionSpeed, AsteroidImpl, PropImpl}
import app.models.game.world.units._
import utils.IdObj

// <editor-fold desc="PROPS">

/* Asteroids can be mined for resources */
case class Asteroid(
  position: Vect2, resources: Resources, extractionSpeed: ExtractionSpeed,
  id: WObject.Id=WObject.newId
) extends Prop with AsteroidImpl
object AsteroidStats extends WObjectStats {
  override val blocksWarp = false
}

/* Rock is an immovable 1x1 obstacle that you can neither move nor see through. */
case class Rock(
  position: Vect2, id: WObject.Id=WObject.newId
) extends Prop {
  type Stats = RockStats.type
  override val stats = RockStats
}
object RockStats extends WObjectStats {
  override val blocksWarp = true
}

/* Brush is an immovable 1x1 object that obstructs visibility, but you can still move
   into it. */
case class Brush(
  position: Vect2, id: WObject.Id=WObject.newId
) extends Prop {
  type Stats = BrushStats.type
  override val stats = BrushStats
}
object BrushStats extends WObjectStats {
  override val blocksWarp = false
  override val blocksVisibility = true
  override val blocksMovement = false
}

/* Crystal is an immovable 1x1 object that obstructs movement & warp zone, but you can
   see through it. */
case class Crystal(
  position: Vect2, id: WObject.Id=WObject.newId
) extends Prop {
  type Stats = CrystalStats.type
  override val stats = CrystalStats
}
object CrystalStats extends WObjectStats {
  override val blocksVisibility = false
  override val blocksWarp = true
}

// </editor-fold>

// <editor-fold desc="BUILDINGS">

/* Main building of a player */
case class WarpGate(
  position: Vect2, owner: Team,
  hp: HP=WarpGateStats.maxHp, id: WObject.Id=WObject.newId
) extends WarpGateImpl with TeamBuilding with GivingActions with GivingPopulation
with SizedWObject with SpecialActionGetResources with AutoSpecial {
  type Stats = WarpGateStats.type
  override val stats = WarpGateStats
}
object WarpGateStats extends BuildingStats with SizedWObjectStats
with GivingActionsStats with GivingPopulationStats with SpecialActionGetResourcesStats
with WarpGateStatsImpl

/* Gives population */
case class PopulationTower(
  position: Vect2, owner: Player,
  warpState: WarpTime=PopulationTowerStats.InitialWarpState,
  hp: HP=PopulationTowerStats.maxHp, id: WObject.Id=WObject.newId
) extends PopulationTowerImpl with GivingPopulation with Building with Warpable {
  type Stats = PopulationTowerStats.type
  override val stats = PopulationTowerStats
}
object PopulationTowerStats extends WBuildingStats
  with GivingPopulationStats
  with PopulationTowerStatsImpl

/* Gives population */
case class ActionTower(
  position: Vect2, owner: Player,
  warpState: WarpTime=ActionTowerStats.InitialWarpState,
  hp: HP=ActionTowerStats.maxHp, id: WObject.Id=WObject.newId
) extends ActionTowerImpl with GivingActions with Building with Warpable {
  type Stats = ActionTowerStats.type
  override val stats = ActionTowerStats
}
object ActionTowerStats extends WBuildingStats
  with GivingActionsStats
  with ActionTowerStatsImpl

/* Gives victory points each turn to its owner */
case class VPTower(
  position: Vect2, owner: Team,
  hp: HP=VPTowerStats.maxHp, id: WObject.Id=WObject.newId
) extends VPTowerImpl with TeamBuilding with GivingActions with RespawnsOnDestruction
with SizedWObject with GivingVictoryPoints {
  type Stats = VPTowerStats.type
  override val stats = VPTowerStats
}
object VPTowerStats extends BuildingStats with GivingActionsStats
  with RespawnsOnDestructionStats with SizedWObjectStats
  with GivingVictoryPointsStats
  with VPTowerStatsImpl

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
object ExtractorStats extends WBuildingStats with SpecialActionStats
  with ExtractorStatsImpl

/* Extends warp zone for players */
case class WarpLinker(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=WarpLinkerStats.maxHp,
  warpState: WarpTime=WarpLinkerStats.InitialWarpState
) extends PlayerBuilding with Building with Warpable {
  type Stats = WarpLinkerStats.type
  override val stats = WarpLinkerStats
}
object WarpLinkerStats extends WBuildingStats with WarpableStats
  with WarpLinkerStatsImpl

/* Defensive tower */
case class LaserTower(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=LaserTowerStats.maxHp,
  xp: XP=LaserTowerStats.InitialXP,
  warpState: WarpTime=LaserTowerStats.InitialWarpState,
  attacksLeft: Attacks=LaserTowerStats.InitialAttacks
) extends LaserTowerImpl with PlayerBuilding with Warpable
with ReactiveFighter with SpecialAction {
  type Stats = LaserTowerStats.type
  override val stats = LaserTowerStats
}
object LaserTowerStats extends LaserTowerStatsImpl with WBuildingStats
  with SpecialActionStats with FighterStats

/* Enemy base */
object SpawnerStats extends BuildingStats with SizedWObjectStats with GivingActionsStats
  with SpecialActionGetResourcesStats with SpawnerStatsImpl
case class Spawner(
  position: Vect2, owner: Bot,
  id: WObject.Id=WObject.newId,
  turns: Int=0, hp: HP=SpawnerStats.maxHp
) extends SpawnerImpl with BotBuilding with TurnCounter with SizedWObject
with SpecialActionGetResources with GivingActions

// </editor-fold>

// <editor-fold desc="UNITS">

// <editor-fold desc="Player">

case class Scout(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=ScoutStats.maxHp,
  movementLeft: Movement=ScoutStats.InitialMovement,
  warpState: WarpTime=ScoutStats.InitialWarpState
) extends WUnit {
  type Stats = ScoutStats.type
  override val stats = ScoutStats
}
object ScoutStats extends WUnitStats with ScoutStatsImpl

case class WarpPrism(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=WarpPrismStats.maxHp,
  movementLeft: Movement=WarpPrismStats.InitialMovement,
  warpState: WarpTime=WarpPrismStats.InitialWarpState
) extends WarpPrismImpl with WUnit with SpecialAction
object WarpPrismStats extends WUnitStats with SpecialActionStats
  with WarpPrismStatsImpl

case class Corvette(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=CorvetteStats.maxHp,
  xp: XP=CorvetteStats.InitialXP,
  attacksLeft: Attacks=CorvetteStats.InitialAttacks,
  movementLeft: Movement=CorvetteStats.InitialMovement,
  warpState: WarpTime=CorvetteStats.InitialWarpState
) extends CorvetteImpl with WUnit with Fighter with SpecialAction
object CorvetteStats extends CorvetteStatsImpl with WFighterUnitStats
  with SpecialActionStats

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
object GunshipStats extends WFighterUnitStats with GunshipStatsImpl

sealed trait RocketFrigateCommon extends PlayerObj with Fighter with SpecialAction {
  type Stats <: RocketFrigateCommonStats
  def onSpecialAction: RocketFrigateCommon
}
sealed trait RocketFrigateCommonStats extends OwnedObjStats with FighterStats
  with SpecialActionStats with RocketFrigateCommonStatsImpl

case class RocketFrigate(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=RocketFrigateStats.maxHp,
  xp: XP=RocketFrigateStats.InitialXP,
  attacksLeft: Attacks=RocketFrigateStats.InitialAttacks,
  movementLeft: Movement=RocketFrigateStats.InitialMovement,
  warpState: WarpTime=RocketFrigateStats.InitialWarpState
) extends RocketFrigateCommon with WUnit with RocketFrigateImpl {
  type Stats = RocketFrigateStats.type
  override val stats = RocketFrigateStats

  override def onSpecialAction =
    RocketFrigateDeployed(position, owner, id, hp, xp, Attacks(0))
}
object RocketFrigateStats extends RocketFrigateCommonStats with WFighterUnitStats
  with RocketFrigateStatsImpl

case class RocketFrigateDeployed(
  position: Vect2, owner: Player, id: WObject.Id, hp: HP, xp: XP, attacksLeft: Attacks
) extends RocketFrigateCommon with RocketFrigateImpl {
  type Stats = RocketFrigateDeployedStats.type
  override val stats = RocketFrigateDeployedStats

  override def onSpecialAction = RocketFrigate(
    position, owner, id, hp, xp, Attacks(0), Movement.zero,
    RocketFrigateStats.warpTime
  )
}
object RocketFrigateDeployedStats extends RocketFrigateCommonStats with RocketFrigateDeployedStatsImpl

// </editor-fold>

// <editor-fold desc="Enemy">

object DroneStats extends WUnitStats with DroneStatsImpl
case class Drone(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=DroneStats.maxHp,
  movementLeft: Movement=DroneStats.InitialMovement,
  warpState: WarpTime=DroneStats.InitialWarpState
) extends WUnit {
  type Stats = DroneStats.type
  override val stats = DroneStats
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
object WaspStats extends WaspStatsImpl with WFighterUnitStats

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
object RayShipStats extends RayShipStatsImpl with WFighterUnitStats

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
object FortressStats extends FortressStatsImpl with WFighterUnitStats

// </editor-fold>

// </editor-fold>

// <editor-fold desc="TRAITS">

/* World object */
sealed trait WObject extends WObjectImpl
sealed trait WObjectStats extends WObjectStatsImpl
object WObject extends WObjectCompanion {
  case class Id(id: UUID) extends AnyVal with IdObj {
    override protected def prefix = "WObjID"
  }
}

/* Because WObject is sealed we cannot extend it in tests :| */
trait WObjectTestRoot extends WObject
trait WObjectStatsTestRoot extends WObjectStats

/* A property in the world */
sealed trait Prop extends WObject with PropImpl

/* Object that belongs to some faction and not just a world prop */
sealed trait OwnedObj extends WObject with OwnedObjImpl
sealed trait OwnedObjStats extends WObjectStats with OwnedObjStatsImpl
object OwnedObj extends OwnedObjCompanion

sealed trait TeamObj extends OwnedObj { val owner: Team }
sealed trait PlayerObj extends OwnedObj { val owner: Player }
sealed trait HumanObj extends PlayerObj { val owner: Human }
sealed trait BotObj extends PlayerObj { val owner: Bot }

/* Gives actions to its owner */
sealed trait GivingActions extends OwnedObj with GivingActionsImpl
sealed trait GivingActionsStats extends OwnedObjStats with GivingActionsStatsImpl

/* Increases population cap to its owner. */
sealed trait GivingPopulation extends OwnedObj with GivingPopulationImpl
sealed trait GivingPopulationStats extends OwnedObjStats with GivingPopulationStatsImpl

/* Gives victory points each turn */
sealed trait GivingVictoryPoints extends OwnedObj with GivingVictoryPointsImpl
sealed trait GivingVictoryPointsStats extends OwnedObjStats with GivingVictoryPointsStatsImpl

/* Objects that can move. All such objects have 1x1 size. */
sealed trait Movable extends OwnedObj with MovableImpl
sealed trait MovableStats extends OwnedObjStats with MovableStatsImpl

/* Objects that can do a special action. */
sealed trait SpecialAction extends OwnedObj with SpecialActionImpl
sealed trait SpecialActionStats extends OwnedObjStats with SpecialActionStatsImpl

/* Special action = get resources. */
sealed trait SpecialActionGetResources extends SpecialAction
  with SpecialActionGetResourcesImpl
sealed trait SpecialActionGetResourcesStats extends SpecialActionStats
  with SpecialActionGetResourcesStatsImpl

/* Objects that are bigger 1x1. Such objects cannot be moved. */
sealed trait SizedWObject extends WObject with SizedWObjectImpl
sealed trait SizedWObjectStats extends WObjectStats with SizedWObjectStatsImpl

/* Has an internal counter that increases at each game turn. */
sealed trait TurnCounter extends WObject with TurnCounterImpl

/* Can attack. */
sealed trait Fighter extends OwnedObj with FighterImpl
object Fighter extends FighterCompanion
sealed trait FighterStats extends OwnedObjStats with FighterStatsImpl

/* Can attack reactively not on owners turn. */
sealed trait ReactiveFighter extends Fighter with ReactiveFighterImpl

/* If destroyed this object should respawn with new owner and hp. */
sealed trait RespawnsOnDestruction extends OwnedObj with RespawnsOnDestructionImpl
sealed trait RespawnsOnDestructionStats extends OwnedObjStats
  with RespawnsOnDestructionStatsImpl

/* If you mix this in into an object, its special action will be used when player misses
 * his turn time limit. Cost of this special action is always 1. */
sealed trait AutoSpecial extends SpecialAction
sealed trait AutoSpecialStats extends SpecialActionStats {
  val specialActionsNeeded = Actions(1)
}

/* Things that can be warped by in a player/bot. */
sealed trait Warpable extends OwnedObj with WarpableImpl
sealed trait WarpableStats extends OwnedObjStats with WarpableStatsImpl
object Warpable extends ToWarpableOps {
  /* Actions needed to warp in something */
  val ActionsNeeded = Actions(1)
}

/* Buildings are stationary */
sealed trait Building extends OwnedObj with BuildingImpl
sealed trait BuildingStats extends OwnedObjStats with BuildingStatsImpl
sealed trait WBuildingStats extends BuildingStats with WarpableStats
  with WBuildingStatsImpl

sealed trait TeamBuilding extends Building with TeamObj
sealed trait PlayerBuilding extends Building with PlayerObj
sealed trait HumanBuilding extends Building with HumanObj
sealed trait BotBuilding extends Building with BotObj

/* World unit - player controlled movable & warpable objects. */
sealed trait WUnit extends WUnitImpl with PlayerObj with Movable with Warpable
sealed trait WUnitStats extends OwnedObjStats with MovableStats with WarpableStats
  with WUnitStatsImpl
sealed trait WFighterUnitStats extends WUnitStats with FighterStats

// </editor-fold>
