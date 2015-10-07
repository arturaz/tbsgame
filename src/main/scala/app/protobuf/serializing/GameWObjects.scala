package app.protobuf.serializing

import app.models.game.world._
import app.models.game.world.props._
import app.models.game.world.units._
import app.models.game.world.buildings._
import netmsg._

import scala.language.implicitConversions

trait GameWObjects { _: GameProto =>
  implicit def convert(extractionSpeed: ExtractionSpeed)
  : game.WObject.Asteroid.ExtractionSpeed = 
    extractionSpeed match {
      case ExtractionSpeed.Slow => game.WObject.Asteroid.ExtractionSpeed.SLOW
      case ExtractionSpeed.Medium => game.WObject.Asteroid.ExtractionSpeed.MEDIUM
      case ExtractionSpeed.Fast => game.WObject.Asteroid.ExtractionSpeed.FAST
    }

  implicit def convert(group: WarpableGroup): game.WObjectStats.Warpable.Group =
    group match {
      case WarpableGroup.Building => game.WObjectStats.Warpable.Group.BUILDING
      case WarpableGroup.Unit => game.WObjectStats.Warpable.Group.UNIT
    }

  /************************************************************************************/

  def base(obj: WObjectStats): game.WObjectStats.Base =
    game.WObjectStats.Base(obj.blocksMovement)

  def base(obj: WObject): game.WObject.Base =
    game.WObject.Base(obj.id, obj.position)

  /************************************************************************************/

  def sized(obj: SizedWObjectStats): game.WObjectStats.SizedObj =
    game.WObjectStats.SizedObj(obj.size)

  /************************************************************************************/

  def owned(obj: OwnedObjStats): game.WObjectStats.OwnedObj =
    game.WObjectStats.OwnedObj(
      maxHp = obj.maxHp, isCritical = obj.isCritical, visibility = obj.visibility,
      kind = obj.kind, isRespawnable = obj.isInstanceOf[RespawnsOnDestructionStats]
    )

  def owned(obj: OwnedObj): game.WObject.OwnedObj =
    game.WObject.OwnedObj(hp = obj.hp, ownerId = obj.owner.id)

  /************************************************************************************/

  def givingActions(obj: GivingActionsStats): game.WObjectStats.GivingActions =
    game.WObjectStats.GivingActions(obj.actionsGiven.value)

  /************************************************************************************/

  def givingPopulation(obj: GivingPopulationStats): game.WObjectStats.GivingPopulation =
    game.WObjectStats.GivingPopulation(obj.populationGiven.value)

  /************************************************************************************/

  def warpable(obj: WarpableStats): game.WObjectStats.Warpable =
    game.WObjectStats.Warpable(
      cost = obj.cost, warpTime = obj.warpTime, populationCost = obj.populationCost,
      group = obj.group
    )

  def warpable(obj: Warpable): game.WObject.Warpable =
    game.WObject.Warpable(obj.warpState)

  /************************************************************************************/

  def specialAction(obj: SpecialActionStats): game.WObjectStats.SpecialAction =
    game.WObjectStats.SpecialAction(obj.specialActionsNeeded)

  /************************************************************************************/

  def fighter(obj: FighterStats): game.WObjectStats.Fighter =
    game.WObjectStats.Fighter(
      attack = obj.attackDamageRange(obj.attack),
      attackOverrides = obj.attackOverrides.map { case (kind, atk) =>
        game.WObjectStats.Fighter.AttackOverride(kind, obj.attackDamageRange(atk))
      }(collection.breakOut),
      attackRange = obj.attackRange, attacks = obj.attacks
    )

  def fighter(obj: Fighter): game.WObject.Fighter =
    game.WObject.Fighter(attacksLeft = obj.attacksLeft, level = obj.level)

  /************************************************************************************/

  def movable(obj: MovableStats): game.WObjectStats.Movable =
    game.WObjectStats.Movable(obj.movement)

  def movable(obj: Movable): game.WObject.Movable =
    game.WObject.Movable(obj.movementLeft)

  /************************************************************************************/

  implicit def convert(obj: AsteroidStats.type): game.WObjectStats.Asteroid =
    game.WObjectStats.Asteroid(base(obj))

  implicit def convert(obj: Asteroid): game.WObject.Asteroid =
    game.WObject.Asteroid(
      base = base(obj), stats = obj.stats, resources = obj.resources,
      extractionSpeed = obj.extractionSpeed
    )

  /************************************************************************************/

  implicit def convert(obj: RockStats.type): game.WObjectStats.Rock =
    game.WObjectStats.Rock(base(obj))

  implicit def convert(obj: Rock): game.WObject.Rock =
    game.WObject.Rock(obj.stats, base(obj))

  /************************************************************************************/

  implicit def convert(obj: CrystalStats.type): game.WObjectStats.Crystal =
    game.WObjectStats.Crystal(base(obj))

  implicit def convert(obj: Crystal): game.WObject.Crystal =
    game.WObject.Crystal(obj.stats, base(obj))

  /************************************************************************************/

  implicit def convert(obj: BrushStats.type): game.WObjectStats.Brush =
    game.WObjectStats.Brush(base(obj))

  implicit def convert(obj: Brush): game.WObject.Brush =
    game.WObject.Brush(obj.stats, base(obj))

  /************************************************************************************/

  implicit def convert(obj: WarpGateStats.type): game.WObjectStats.WarpGate =
    game.WObjectStats.WarpGate(
      base = base(obj), givingActions = givingActions(obj),
      givingPopulation = givingPopulation(obj), owned = owned(obj), sized = sized(obj),
      specialAction = specialAction(obj)
    )

  implicit def convert(obj: WarpGate): game.WObject.WarpGate =
    game.WObject.WarpGate(base = base(obj), stats = obj.stats, owned = owned(obj))

  /************************************************************************************/

  implicit def convert(obj: ExtractorStats.type): game.WObjectStats.Extractor =
    game.WObjectStats.Extractor(
      base = base(obj), owned = owned(obj), specialAction = specialAction(obj),
      warpable = warpable(obj), specialExtractsFixed = obj.specialExtractsFixed,
      specialExtractsPercentage = obj.specialExtractsPercentage
    )

  implicit def convert(obj: Extractor): game.WObject.Extractor =
    game.WObject.Extractor(
      base = base(obj), stats = obj.stats, owned = owned(obj), warpable = warpable(obj)
    )

  /************************************************************************************/

  implicit def convert(obj: PopulationTowerStats.type): game.WObjectStats.PopulationTower =
    game.WObjectStats.PopulationTower(
      base = base(obj), owned = owned(obj), givingPopulation = givingPopulation(obj),
      warpable = warpable(obj)
    )

  implicit def convert(obj: PopulationTower): game.WObject.PopulationTower =
    game.WObject.PopulationTower(
      base = base(obj), stats = obj.stats, owned = owned(obj), warpable = warpable(obj)
    )

  /************************************************************************************/

  implicit def convert(obj: ActionTowerStats.type): game.WObjectStats.ActionTower =
    game.WObjectStats.ActionTower(
      base = base(obj), owned = owned(obj), givingActions = givingActions(obj),
      warpable = warpable(obj)
    )

  implicit def convert(obj: ActionTower): game.WObject.ActionTower =
    game.WObject.ActionTower(
      base = base(obj), stats = obj.stats, owned = owned(obj), warpable = warpable(obj)
    )

  /************************************************************************************/

  implicit def convert(obj: WarpLinkerStats.type): game.WObjectStats.WarpLinker =
    game.WObjectStats.WarpLinker(
      base = base(obj), owned = owned(obj), warpable = warpable(obj)
    )

  implicit def convert(obj: WarpLinker): game.WObject.WarpLinker =
    game.WObject.WarpLinker(
      base = base(obj), stats = obj.stats, owned = owned(obj), warpable = warpable(obj)
    )

  /************************************************************************************/

  implicit def convert(obj: SpawnerStats.type): game.WObjectStats.Spawner =
    game.WObjectStats.Spawner(base = base(obj), owned = owned(obj), sized = sized(obj))

  implicit def convert(obj: Spawner): game.WObject.Spawner =
    game.WObject.Spawner(base = base(obj), stats = obj.stats, owned = owned(obj))

  /************************************************************************************/

  implicit def convert(obj: LaserTowerStats.type): game.WObjectStats.LaserTower =
    game.WObjectStats.LaserTower(
      base = base(obj), owned = owned(obj), fighter = fighter(obj), 
      specialAction = specialAction(obj), warpable = warpable(obj)
    )

  implicit def convert(obj: LaserTower): game.WObject.LaserTower =
    game.WObject.LaserTower(
      base = base(obj), stats = obj.stats, owned = owned(obj), fighter = fighter(obj), 
      warpable = warpable(obj)
    )

  /************************************************************************************/

  implicit def convert(obj: CorvetteStats.type): game.WObjectStats.Corvette =
    game.WObjectStats.Corvette(
      base = base(obj), owned = owned(obj), fighter = fighter(obj), 
      specialAction = specialAction(obj), warpable = warpable(obj),
      movable = movable(obj), specialMovementAdded = obj.specialMovementAdded
    )

  implicit def convert(obj: Corvette): game.WObject.Corvette =
    game.WObject.Corvette(
      base = base(obj), stats = obj.stats, owned = owned(obj), fighter = fighter(obj),
      warpable = warpable(obj), movable = movable(obj)
    )

  /************************************************************************************/

  implicit def convert(obj: DroneStats.type): game.WObjectStats.Drone =
    game.WObjectStats.Drone(
      base = base(obj), owned = owned(obj),
      warpable = warpable(obj), movable = movable(obj)
    )

  implicit def convert(obj: Drone): game.WObject.Drone =
    game.WObject.Drone(
      base = base(obj), stats = obj.stats, owned = owned(obj),
      warpable = warpable(obj), movable = movable(obj)
    )

  /************************************************************************************/

  implicit def convert(obj: WaspStats.type): game.WObjectStats.Wasp =
    game.WObjectStats.Wasp(
      base = base(obj), owned = owned(obj), fighter = fighter(obj), 
      warpable = warpable(obj), movable = movable(obj)
    )

  implicit def convert(obj: Wasp): game.WObject.Wasp =
    game.WObject.Wasp(
      base = base(obj), stats = obj.stats, owned = owned(obj), fighter = fighter(obj), 
      warpable = warpable(obj), movable = movable(obj)
    )

  /************************************************************************************/

  implicit def convert(obj: WarpPrismStats.type): game.WObjectStats.WarpPrism =
    game.WObjectStats.WarpPrism(
      base = base(obj), owned = owned(obj),
      warpable = warpable(obj), movable = movable(obj), specialAction = specialAction(obj)
    )

  implicit def convert(obj: WarpPrism): game.WObject.WarpPrism =
    game.WObject.WarpPrism(
      base = base(obj), stats = obj.stats, owned = owned(obj),
      warpable = warpable(obj), movable = movable(obj)
    )

  /************************************************************************************/

  implicit def convert(obj: ScoutStats.type): game.WObjectStats.Scout =
    game.WObjectStats.Scout(
      base = base(obj), owned = owned(obj), warpable = warpable(obj), 
      movable = movable(obj)
    )

  implicit def convert(obj: Scout): game.WObject.Scout =
    game.WObject.Scout(
      base = base(obj), stats = obj.stats, owned = owned(obj), warpable = warpable(obj),
      movable = movable(obj)
    )

  /************************************************************************************/

  implicit def convert(obj: RayShipStats.type): game.WObjectStats.RayShip =
    game.WObjectStats.RayShip(
      base = base(obj), owned = owned(obj), fighter = fighter(obj), 
      warpable = warpable(obj), movable = movable(obj)
    )

  implicit def convert(obj: RayShip): game.WObject.RayShip =
    game.WObject.RayShip(
      base = base(obj), stats = obj.stats, owned = owned(obj), fighter = fighter(obj), 
      warpable = warpable(obj), movable = movable(obj)
    )

  /************************************************************************************/

  implicit def convert(obj: RocketFrigateCommonStats): game.WObjectStats.RocketFrigate =
    game.WObjectStats.RocketFrigate(
      base = base(obj), owned = owned(obj), fighter = fighter(obj), 
      warpable = warpable(obj), movable = movable(obj), specialAction = specialAction(obj),
      deployed = obj.deployed
    )

  implicit def convert(obj: RocketFrigate): game.WObject.RocketFrigate =
    game.WObject.RocketFrigate(
      base = base(obj), stats = obj.stats, owned = owned(obj), fighter = fighter(obj), 
      warpable = warpable(obj), movable = movable(obj)
    )

  /************************************************************************************/

  implicit def convert(obj: GunshipStats.type): game.WObjectStats.Gunship =
    game.WObjectStats.Gunship(
      base = base(obj), owned = owned(obj), fighter = fighter(obj), 
      warpable = warpable(obj), movable = movable(obj)
    )

  implicit def convert(obj: Gunship): game.WObject.Gunship =
    game.WObject.Gunship(
      base = base(obj), stats = obj.stats, owned = owned(obj), fighter = fighter(obj), 
      warpable = warpable(obj), movable = movable(obj)
    )

  /************************************************************************************/

  implicit def convert(obj: FortressStats.type): game.WObjectStats.Fortress =
    game.WObjectStats.Fortress(
      base = base(obj), owned = owned(obj), fighter = fighter(obj), 
      warpable = warpable(obj), movable = movable(obj)
    )

  implicit def convert(obj: Fortress): game.WObject.Fortress =
    game.WObject.Fortress(
      base = base(obj), stats = obj.stats, owned = owned(obj), fighter = fighter(obj), 
      warpable = warpable(obj), movable = movable(obj)
    )

  /************************************************************************************/

  implicit def convert(obj: VPTowerStats.type): game.WObjectStats.VpTower =
    game.WObjectStats.VpTower(
      base = base(obj), owned = owned(obj), sized = sized(obj), 
      givingActions = givingActions(obj)
    )

  implicit def convert(obj: VPTower): game.WObject.VpTower =
    game.WObject.VpTower(base = base(obj), stats = obj.stats, owned = owned(obj))

  /************************************************************************************/

  implicit def convert(obj: WObject): game.WObject =
    obj match {
      case o: Asteroid => game.WObject(asteroid = Some(o))
      case o: Rock => game.WObject(rock = Some(o))
      case o: Crystal => game.WObject(crystal = Some(o))
      case o: Brush => game.WObject(brush = Some(o))
      case o: WarpGate => game.WObject(warpGate = Some(o))
      case o: Extractor => game.WObject(extractor = Some(o))
      case o: PopulationTower => game.WObject(populationTower = Some(o))
      case o: ActionTower => game.WObject(actionTower = Some(o))
      case o: WarpLinker => game.WObject(warpLinker = Some(o))
      case o: Spawner => game.WObject(spawner = Some(o))
      case o: LaserTower => game.WObject(laserTower = Some(o))
      case o: Corvette => game.WObject(corvette = Some(o))
      case o: WarpPrism => game.WObject(warpPrism = Some(o))
      case o: Wasp => game.WObject(wasp = Some(o))
      case o: Drone => game.WObject(drone = Some(o))
      case o: Scout => game.WObject(scout = Some(o))
      case o: RayShip => game.WObject(rayShip = Some(o))
      case o: RocketFrigate => game.WObject(rocketFrigate = Some(o))
      case o: Gunship => game.WObject(gunship = Some(o))
      case o: Fortress => game.WObject(fortress = Some(o))
      case o: VPTower => game.WObject(vpTower = Some(o))
      case o: WObjectTestRoot => throw new Exception(
        s"$o should never be used outside of tests!"
      )
    }
  
  implicit def convert(obj: WObjectStats): game.WObjectStats =
    obj match {
      case o: AsteroidStats.type => game.WObjectStats(asteroid = Some(o))
      case o: RockStats.type => game.WObjectStats(rock = Some(o))
      case o: CrystalStats.type => game.WObjectStats(crystal = Some(o))
      case o: BrushStats.type => game.WObjectStats(brush = Some(o))
      case o: WarpGateStats.type => game.WObjectStats(warpGate = Some(o))
      case o: ExtractorStats.type => game.WObjectStats(extractor = Some(o))
      case o: PopulationTowerStats.type => game.WObjectStats(populationTower = Some(o))
      case o: ActionTowerStats.type => game.WObjectStats(actionTower = Some(o))
      case o: WarpLinkerStats.type => game.WObjectStats(warpLinker = Some(o))
      case o: SpawnerStats.type => game.WObjectStats(spawner = Some(o))
      case o: LaserTowerStats.type => game.WObjectStats(laserTower = Some(o))
      case o: CorvetteStats.type => game.WObjectStats(corvette = Some(o))
      case o: WarpPrismStats.type => game.WObjectStats(warpPrism = Some(o))
      case o: WaspStats.type => game.WObjectStats(wasp = Some(o))
      case o: DroneStats.type => game.WObjectStats(drone = Some(o))
      case o: ScoutStats.type => game.WObjectStats(scout = Some(o))
      case o: RayShipStats.type => game.WObjectStats(rayShip = Some(o))
      case o: RocketFrigateCommonStats => game.WObjectStats(rocketFrigate = Some(o))
      case o: GunshipStats.type => game.WObjectStats(gunship = Some(o))
      case o: FortressStats.type => game.WObjectStats(fortress = Some(o))
      case o: VPTowerStats.type => game.WObjectStats(vpTower = Some(o))
      case o: WObjectStatsTestRoot => throw new Exception(
        s"$o should never be used outside of tests!"
      )
    }

  def convertWarpableStats(statsTO: TraversableOnce[WarpableStats])
  : game.WarpableObjectStats =
    statsTO.foldLeft(game.WarpableObjectStats()) {
      case (b, s: ExtractorStats.type) => b.withExtractor(s)
      case (b, s: WarpLinkerStats.type) => b.withWarpLinker(s)
      case (b, s: LaserTowerStats.type) => b.withLaserTower(s)
      case (b, s: PopulationTowerStats.type) => b.withPopulationTower(s)
      case (b, s: ActionTowerStats.type) => b.withActionTower(s)
      case (b, s: CorvetteStats.type) => b.withCorvette(s)
      case (b, s: DroneStats.type) => b.withDrone(s)
      case (b, s: WaspStats.type) => b.withWasp(s)
      case (b, s: WarpPrismStats.type) => b.withWarpPrism(s)
      case (b, s: ScoutStats.type) => b.withScout(s)
      case (b, s: RayShipStats.type) => b.withRayShip(s)
      case (b, s: RocketFrigateCommonStats) => b.withRocketFrigate(s)
      case (b, s: GunshipStats.type) => b.withGunship(s)
      case (b, s: FortressStats.type) => b.withFortress(s)
    }
}
