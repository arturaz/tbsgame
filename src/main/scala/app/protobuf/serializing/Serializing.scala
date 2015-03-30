package app.protobuf.serializing

import java.util.UUID

import akka.util.ByteString
import app.actors.NetClient
import app.actors.game.GameActor
import app.algorithms.Pathfinding.Path
import app.models.game.events._
import app.models.game.{Game => _, _}
import app.models.game.world._
import app.models.game.world.buildings._
import app.models.game.world.units._
import app.models.game.world.props._
import netmsg._
import org.joda.time.DateTime
import spire.math.Rational
import utils.data.Timeframe
import utils._
import scala.collection.JavaConverters._
import implicits._
import language.implicitConversions

object Serializing {
  /* Helpers */

  implicit def convert[A, B]
  (seq: Iterable[A])(implicit converter: A => B): java.lang.Iterable[_ <: B] =
    seq.toIterable.map(converter).asJava

  def valWithMax(current: Int, maximum: Int): Base.ValWithMax =
    Base.ValWithMax.newBuilder().setCurrent(current).setMaximum(maximum).build()

  implicit def convert[A <: IntValueClass[A]](vwm: ValWithMax[A]): Base.ValWithMax =
    Base.ValWithMax.newBuilder().setCurrent(vwm.value).setMaximum(vwm.max).build()

  implicit def convert(dateTime: DateTime): Base.Timestamp =
    Base.Timestamp.newBuilder().setTimestamp(dateTime.getMillis).build()

  implicit def convert(tf: Timeframe): Base.Timeframe =
    Base.Timeframe.newBuilder().setStart(tf.start).setEnd(tf.end).build()

  implicit def convert(extractionSpeed: ExtractionSpeed): Game.WObject.Asteroid.ExtractionSpeed =
    extractionSpeed match {
      case ExtractionSpeed.Slow => Game.WObject.Asteroid.ExtractionSpeed.SLOW
      case ExtractionSpeed.Medium => Game.WObject.Asteroid.ExtractionSpeed.MEDIUM
      case ExtractionSpeed.Fast => Game.WObject.Asteroid.ExtractionSpeed.FAST
    }

  def convertInit(extractionSpeed: ExtractionSpeed): Game.MInit.ExtractionSpeedRate =
    Game.MInit.ExtractionSpeedRate.newBuilder().
      setExtractionSpeed(extractionSpeed).
      setResourcesPerTurn(extractionSpeed.resourcesPerTurn).build()

  /* Data */

  implicit def convert(id: UUID): Base.UUID =
    Base.UUID.newBuilder().
      setLeastSignificant(id.getLeastSignificantBits).
      setMostSignificant(id.getMostSignificantBits).build()

  implicit def convert(v: IntValueClass[_]): Int = v.value
  implicit def convert(v: DoubleValueClass[_]): Double = v.value
  implicit def convert(v: FloatValueClass[_]): Float = v.value
  implicit def convert(v: RationalValueClass[_]): Base.Rational = v.value
  implicit def convert(r: Rational): Base.Rational =
    Base.Rational.newBuilder().
      setNumerator(r.numeratorAsLong).setDenominator(r.denominatorAsLong).build()

  implicit def convert(range: Range.Inclusive): Base.Range =
    Base.Range.newBuilder().setStart(range.start).setEnd(range.end).build()

  implicit def convert(v: Vect2): Base.Vect2 =
    netmsg.base.Vect2(v.x, v.y) |> netmsg.base.Vect2.toJavaProto

  implicit def convert(b: Bounds): Base.Bounds =
    Base.Bounds.newBuilder().setStart(b.start).setEnd(b.end).build()

  implicit def convert(id: WObject.Id): Game.WObjID =
    Game.WObjID.newBuilder().setId(id.id).build()

  implicit def convert(id: Player.Id): Game.PlayerID =
    Game.PlayerID.newBuilder().setId(id.id).build()

  implicit def convert(id: Team.Id): Game.TeamID =
    Game.TeamID.newBuilder().setId(id.id).build()

  implicit def convert(id: Owner.Id): Game.OwnerID =
    Game.OwnerID.newBuilder().mapVal { b => id match {
      case pid: Player.Id => b.setPlayerId(pid)
      case tid: Team.Id => b.setTeamId(tid)
    } }.build()

  implicit def convert(player: Player): Game.Player =
    Game.Player.newBuilder().
      setId(player.id).setName(player.asHuman.map(_.name).getOrElse("Bot")).
      setTeamId(player.team.id).build()

  implicit def convert(state: HumanState): Game.PlayerState =
    Game.PlayerState.newBuilder().
      setActions(state.gameState.actions).setPopulation(state.population).
      setResources(state.resources).setTurnEnded(!state.gameState.activity.canAct).
      build()

  def convert(player: Player, state: Option[HumanState]): Game.InitPlayer =
    Game.InitPlayer.newBuilder().setPlayer(convert(player)).
      mapVal { b => state.fold2(b, b.setState(_)) }.build()

  def convert(t: (Player, Option[HumanState])): Game.InitPlayer = convert(t._1, t._2)

  implicit def convert(objectives: RemainingObjectives): Game.Objectives =
    Game.Objectives.newBuilder().mapVal { b =>
      objectives.gatherResources.fold2(b, b.setGatherResourcesLeft(_))
    }.mapVal { b =>
      objectives.collectVps.fold2(b, b.setCollectVpsLeft(_))
    }.mapVal { b =>
      objectives.destroyAllCriticalObjects.fold2(b, b.setDestroyAllCriticalObjectsLeft(_))
    }.build()

  implicit def convert(team: Team): Game.Team =
    Game.Team.newBuilder().setId(team.id).build()

  /************************************************************************************/

  implicit def convert(group: WarpableGroup): Game.WObjectStats.Warpable.Group =
    group match {
      case WarpableGroup.Building => Game.WObjectStats.Warpable.Group.BUILDING
      case WarpableGroup.Unit => Game.WObjectStats.Warpable.Group.UNIT
    }

  /************************************************************************************/

  def base(obj: WObjectStats): Game.WObjectStats.Base =
    Game.WObjectStats.Base.newBuilder().setBlocksMovement(obj.blocksMovement).build()

  def base(obj: WObject): Game.WObject.Base =
    Game.WObject.Base.newBuilder().setId(obj.id).setPosition(obj.position).build()

  /************************************************************************************/

  def sized(obj: SizedWObjectStats): Game.WObjectStats.SizedObj =
    Game.WObjectStats.SizedObj.newBuilder().setSize(obj.size).build()

  /************************************************************************************/

  def owned(obj: OwnedObjStats): Game.WObjectStats.OwnedObj =
    Game.WObjectStats.OwnedObj.newBuilder().
      setMaxHp(obj.maxHp).setIsCritical(obj.isCritical).
      setVisibility(obj.visibility).
      setKind(obj.kind |> convert |> game.WObjKind.toJavaValue).
      setIsRespawnable(obj.isInstanceOf[RespawnsOnDestructionStats]).
      build()

  def owned(obj: OwnedObj): Game.WObject.OwnedObj =
    Game.WObject.OwnedObj.newBuilder().
      setHp(obj.hp).setOwnerId(obj.owner.id).build()

  /************************************************************************************/

  def givingActions(obj: GivingActionsStats): Game.WObjectStats.GivingActions =
    Game.WObjectStats.GivingActions.newBuilder().
      setActionsGiven(obj.actionsGiven.value).build()

  /************************************************************************************/

  def givingPopulation(obj: GivingPopulationStats): Game.WObjectStats.GivingPopulation =
    Game.WObjectStats.GivingPopulation.newBuilder().
      setPopulationGiven(obj.populationGiven.value).build()

  /************************************************************************************/

  def warpable(obj: WarpableStats): Game.WObjectStats.Warpable =
    Game.WObjectStats.Warpable.newBuilder().setCost(obj.cost).
      setWarpTime(obj.warpTime).setPopulationCost(obj.populationCost).
      setGroup(obj.group).build()

  def warpable(obj: Warpable): Game.WObject.Warpable =
    Game.WObject.Warpable.newBuilder().setWarpState(obj.warpState).build()

  /************************************************************************************/

  def specialAction(obj: SpecialActionStats): Game.WObjectStats.SpecialAction =
    Game.WObjectStats.SpecialAction.newBuilder().
      setActionsNeeded(obj.specialActionsNeeded).build()

  /************************************************************************************/

  def fighter(obj: FighterStats): Game.WObjectStats.Fighter =
    Game.WObjectStats.Fighter.newBuilder().
      setAttack(obj.attackDamageRange).setAttackRange(obj.attackRange).
      setAttacks(obj.attacks).
      build()

  def fighter(obj: Fighter): Game.WObject.Fighter =
    Game.WObject.Fighter.newBuilder().
      setAttacksLeft(obj.attacksLeft).setLevel(obj.level).build()

  /************************************************************************************/

  def movable(obj: MovableStats): Game.WObjectStats.Movable =
    Game.WObjectStats.Movable.newBuilder().setMovementRange(obj.movement).build()

  def movable(obj: Movable): Game.WObject.Movable =
    Game.WObject.Movable.newBuilder().setMovement(obj.movementLeft).build()

  /************************************************************************************/

  implicit def convert(obj: AsteroidStats.type): Game.WObjectStats.Asteroid =
    Game.WObjectStats.Asteroid.newBuilder().setBase(base(obj)).build()

  implicit def convert(obj: Asteroid): Game.WObject.Asteroid =
    Game.WObject.Asteroid.newBuilder().setBase(base(obj)).setStats(obj.stats).
      setResources(obj.resources).setExtractionSpeed(obj.extractionSpeed).build()

  /************************************************************************************/

  implicit def convert(obj: RockStats.type): Game.WObjectStats.Rock =
    Game.WObjectStats.Rock.newBuilder().setBase(base(obj)).build()

  implicit def convert(obj: Rock): Game.WObject.Rock =
    Game.WObject.Rock.newBuilder().setBase(base(obj)).setStats(obj.stats).build()

  /************************************************************************************/

  implicit def convert(obj: CrystalStats.type): Game.WObjectStats.Crystal =
    Game.WObjectStats.Crystal.newBuilder().setBase(base(obj)).build()

  implicit def convert(obj: Crystal): Game.WObject.Crystal =
    Game.WObject.Crystal.newBuilder().setBase(base(obj)).setStats(obj.stats).build()

  /************************************************************************************/

  implicit def convert(obj: BrushStats.type): Game.WObjectStats.Brush =
    Game.WObjectStats.Brush.newBuilder().setBase(base(obj)).build()

  implicit def convert(obj: Brush): Game.WObject.Brush =
    Game.WObject.Brush.newBuilder().setBase(base(obj)).setStats(obj.stats).build()

  /************************************************************************************/

  implicit def convert(obj: WarpGateStats.type): Game.WObjectStats.WarpGate =
    Game.WObjectStats.WarpGate.newBuilder().setBase(base(obj))
      .setGivingActions(givingActions(obj))
      .setGivingPopulation(givingPopulation(obj)).setOwned(owned(obj))
      .setSized(sized(obj)).setSpecialAction(specialAction(obj)).build()

  implicit def convert(obj: WarpGate): Game.WObject.WarpGate =
    Game.WObject.WarpGate.newBuilder().setBase(base(obj)).setStats(obj.stats)
      .setOwned(owned(obj)).build()

  /************************************************************************************/

  implicit def convert(obj: ExtractorStats.type): Game.WObjectStats.Extractor =
    Game.WObjectStats.Extractor.newBuilder()
      .setBase(base(obj)).setOwned(owned(obj)).setSpecialAction(specialAction(obj))
      .setWarpable(warpable(obj)).setSpecialExtractsFixed(obj.specialExtractsFixed).
      setSpecialExtractsPercentage(obj.specialExtractsPercentage).build()

  implicit def convert(obj: Extractor): Game.WObject.Extractor =
    Game.WObject.Extractor.newBuilder().setBase(base(obj)).setStats(obj.stats)
      .setOwned(owned(obj)).setWarpable(warpable(obj)).build()

  /************************************************************************************/

  implicit def convert(obj: WarpLinkerStats.type): Game.WObjectStats.WarpLinker =
    Game.WObjectStats.WarpLinker.newBuilder().
      setBase(base(obj)).setOwned(owned(obj)).setWarpable(warpable(obj)).build()

  implicit def convert(obj: WarpLinker): Game.WObject.WarpLinker =
    Game.WObject.WarpLinker.newBuilder().setBase(base(obj)).setStats(obj.stats)
      .setOwned(owned(obj)).setWarpable(warpable(obj)).build()

  /************************************************************************************/

  implicit def convert(obj: SpawnerStats.type): Game.WObjectStats.Spawner =
    Game.WObjectStats.Spawner.newBuilder().
      setBase(base(obj)).setOwned(owned(obj)).setSized(sized(obj)).build()

  implicit def convert(obj: Spawner): Game.WObject.Spawner =
    Game.WObject.Spawner.newBuilder().setBase(base(obj)).setStats(obj.stats)
      .setOwned(owned(obj)).build()

  /************************************************************************************/

  implicit def convert(obj: LaserTowerStats.type): Game.WObjectStats.LaserTower =
    Game.WObjectStats.LaserTower.newBuilder()
      .setBase(base(obj)).setOwned(owned(obj)).setFighter(fighter(obj))
      .setSpecialAction(specialAction(obj)).setWarpable(warpable(obj))
      .build()

  implicit def convert(obj: LaserTower): Game.WObject.LaserTower =
    Game.WObject.LaserTower.newBuilder().setBase(base(obj)).setStats(obj.stats)
      .setOwned(owned(obj)).setFighter(fighter(obj)).setWarpable(warpable(obj)).build()

  /************************************************************************************/

  implicit def convert(obj: CorvetteStats.type): Game.WObjectStats.Corvette =
    Game.WObjectStats.Corvette.newBuilder()
      .setBase(base(obj)).setOwned(owned(obj)).setFighter(fighter(obj))
      .setSpecialAction(specialAction(obj)).setWarpable(warpable(obj))
      .setMovable(movable(obj)).setSpecialMovementAdded(obj.specialMovementAdded)
      .build()

  implicit def convert(obj: Corvette): Game.WObject.Corvette =
    Game.WObject.Corvette.newBuilder().setBase(base(obj)).setStats(obj.stats)
      .setOwned(owned(obj)).setFighter(fighter(obj)).setWarpable(warpable(obj))
      .setMovable(movable(obj)).build()

  /************************************************************************************/

  implicit def convert(obj: WaspStats.type): Game.WObjectStats.Wasp =
    Game.WObjectStats.Wasp.newBuilder()
      .setBase(base(obj)).setOwned(owned(obj)).setFighter(fighter(obj))
      .setWarpable(warpable(obj)).setMovable(movable(obj))
      .build()

  implicit def convert(obj: Wasp): Game.WObject.Wasp =
    Game.WObject.Wasp.newBuilder().setBase(base(obj)).setStats(obj.stats)
      .setOwned(owned(obj)).setFighter(fighter(obj)).setWarpable(warpable(obj))
      .setMovable(movable(obj)).build()

  /************************************************************************************/

  implicit def convert(obj: ScoutStats.type): Game.WObjectStats.Scout =
    Game.WObjectStats.Scout.newBuilder()
      .setBase(base(obj)).setOwned(owned(obj)).setWarpable(warpable(obj))
      .setMovable(movable(obj)).build()

  implicit def convert(obj: Scout): Game.WObject.Scout =
    Game.WObject.Scout.newBuilder().setBase(base(obj)).setStats(obj.stats)
      .setOwned(owned(obj)).setWarpable(warpable(obj)).setMovable(movable(obj)).build()

  /************************************************************************************/

  implicit def convert(obj: RayShipStats.type): Game.WObjectStats.RayShip =
    Game.WObjectStats.RayShip.newBuilder()
      .setBase(base(obj)).setOwned(owned(obj)).setFighter(fighter(obj))
      .setWarpable(warpable(obj)).setMovable(movable(obj)).build()

  implicit def convert(obj: RayShip): Game.WObject.RayShip =
    Game.WObject.RayShip.newBuilder().setBase(base(obj)).setStats(obj.stats)
      .setOwned(owned(obj)).setFighter(fighter(obj)).setWarpable(warpable(obj))
      .setMovable(movable(obj)).build()

  /************************************************************************************/

  implicit def convert(obj: RocketFrigateStats.type): Game.WObjectStats.RocketFrigate =
    Game.WObjectStats.RocketFrigate.newBuilder()
      .setBase(base(obj)).setOwned(owned(obj)).setFighter(fighter(obj))
      .setWarpable(warpable(obj)).setMovable(movable(obj)).build()

  implicit def convert(obj: RocketFrigate): Game.WObject.RocketFrigate =
    Game.WObject.RocketFrigate.newBuilder().setBase(base(obj)).setStats(obj.stats)
      .setOwned(owned(obj)).setFighter(fighter(obj)).setWarpable(warpable(obj))
      .setMovable(movable(obj)).build()

  /************************************************************************************/

  implicit def convert(obj: GunshipStats.type): Game.WObjectStats.Gunship =
    Game.WObjectStats.Gunship.newBuilder()
      .setBase(base(obj)).setOwned(owned(obj)).setFighter(fighter(obj))
      .setWarpable(warpable(obj)).setMovable(movable(obj))
      .build()

  implicit def convert(obj: Gunship): Game.WObject.Gunship =
    Game.WObject.Gunship.newBuilder().setBase(base(obj)).setStats(obj.stats)
      .setOwned(owned(obj)).setFighter(fighter(obj)).setWarpable(warpable(obj))
      .setMovable(movable(obj)).build()

  /************************************************************************************/

  implicit def convert(obj: FortressStats.type): Game.WObjectStats.Fortress =
    Game.WObjectStats.Fortress.newBuilder()
      .setBase(base(obj)).setOwned(owned(obj)).setFighter(fighter(obj))
      .setWarpable(warpable(obj)).setMovable(movable(obj))
      .build()

  implicit def convert(obj: Fortress): Game.WObject.Fortress =
    Game.WObject.Fortress.newBuilder().setBase(base(obj)).setStats(obj.stats)
      .setOwned(owned(obj)).setFighter(fighter(obj)).setWarpable(warpable(obj))
      .setMovable(movable(obj)).build()

  /************************************************************************************/

  implicit def convert(obj: VPTowerStats.type): Game.WObjectStats.VpTower =
    Game.WObjectStats.VpTower.newBuilder()
      .setBase(base(obj)).setOwned(owned(obj)).setSized(sized(obj))
      .setGivingActions(givingActions(obj)).build()

  implicit def convert(obj: VPTower): Game.WObject.VpTower =
    Game.WObject.VpTower.newBuilder().setBase(base(obj)).setStats(obj.stats)
      .setOwned(owned(obj)).build()

  /************************************************************************************/

  implicit def convert(obj: WObject): Game.WObject =
    Game.WObject.newBuilder().mapVal { b => obj match {
      case o: Asteroid => b.setAsteroid(o)
      case o: Rock => b.setRock(o)
      case o: Crystal => b.setCrystal(o)
      case o: Brush => b.setBrush(o)
      case o: WarpGate => b.setWarpGate(o)
      case o: Extractor => b.setExtractor(o)
      case o: WarpLinker => b.setWarpLinker(o)
      case o: Spawner => b.setSpawner(o)
      case o: LaserTower => b.setLaserTower(o)
      case o: Corvette => b.setCorvette(o)
      case o: Wasp => b.setWasp(o)
      case o: Scout => b.setScout(o)
      case o: RayShip => b.setRayShip(o)
      case o: RocketFrigate => b.setRocketFrigate(o)
      case o: Gunship => b.setGunship(o)
      case o: Fortress => b.setFortress(o)
      case o: VPTower => b.setVpTower(o)
      case o: WObjectTestRoot => b
    } }.build()

  def convertWarpableStats(statsTO: TraversableOnce[WarpableStats])
  : Game.WarpableObjectStats =
    statsTO.foldLeft(Game.WarpableObjectStats.newBuilder()) {
      // TODO: sealed traits would benefit us here as well.
      case (b, s: ExtractorStats.type) => b.setExtractor(s)
      case (b, s: WarpLinkerStats.type) => b.setWarpLinker(s)
      case (b, s: LaserTowerStats.type) => b.setLaserTower(s)
      case (b, s: CorvetteStats.type) => b.setCorvette(s)
      case (b, s: WaspStats.type) => b.setWasp(s)
      case (b, s: ScoutStats.type) => b.setScout(s)
      case (b, s: RayShipStats.type) => b.setRayShip(s)
      case (b, s: RocketFrigateStats.type) => b.setRocketFrigate(s)
      case (b, s: GunshipStats.type) => b.setGunship(s)
      case (b, s: FortressStats.type) => b.setFortress(s)
    }.build()

  /************************************************************************************/

  implicit def convert(attack: Attack): Game.Attack =
    netmsg.game.Attack.toJavaProto(
      netmsg.game.Attack(attack.attackerRoll, attack.successful)
    )

  implicit def convert(kind: WObjKind): game.WObjKind = kind match {
    case WObjKind.Light => game.WObjKind.LIGHT
    case WObjKind.Medium => game.WObjKind.MEDIUM
    case WObjKind.Heavy => game.WObjKind.HEAVY
  }

  def attackMultiplier(from: WObjKind, to: WObjKind): Game.MInit.AttackMultiplier =
    game.MInit.AttackMultiplier(from, to, from.multiplierAt(to).toFloat) |>
    game.MInit.AttackMultiplier.toJavaProto

  implicit def convert(data: GameActor.Out.Movement.Immovable)
  : Game.MMovement.Positions =
    Game.MMovement.Positions.newBuilder().addAllPosition(convert(data.points)).build()

  implicit def convert(path: Path): Option[Game.MMovement.Path] = {
    val tail = path.vects.tail
    if (tail.isEmpty) None
    else Some(Game.MMovement.Path.newBuilder().addAllPosition(convert(tail)).build())
  }

  implicit def convert(data: GameActor.Out.Movement.Movable): Game.MMovement.Paths =
    Game.MMovement.Paths.newBuilder().
      addAllPath(data.paths.flatMap(path => convert(path)).asJava).build()

  //    implicit def convertObjectives(init: GameActor.Out.Init): Game.Objectives =
  //      Game.Objectives.newBuilder().
  //        mapVal { b =>
  //          init.objectives.gatherResources.fold2(b, o => b.setGatherResourcesLeft(
  //            valWithMax(o.remaining(init.game, init.selfTeam).value, o.resources.value)
  //          ))
  //        }.
  //        mapVal { b =>
  //          init.objectives.collectVps.fold2(b, o => b.setCollectVpsLeft(
  //            valWithMax(o.remaining(init.game, init.selfTeam).value, o.vps.value)
  //          ))
  //        }.
  //        mapVal { b =>
  //          init.objectives.destroyAllCriticalObjects.fold2(
  //            b, o => b.setDestroyAllCriticalObjectsLeft(
  //              valWithMax(o.remaining(init.game, init.selfTeam).value, o..value)
  //            )
  //          )
  //        }.
  //        build()

  /* Events */

  implicit def convert(evt: TurnStartedEvt): Game.TurnStartedEvt =
    Game.TurnStartedEvt.newBuilder().setTeamId(evt.team.id).build()

  implicit def convert(evt: TurnEndedEvt): Game.TurnEndedEvt =
    Game.TurnEndedEvt.newBuilder().setTeamId(evt.team.id).build()

  implicit def convert(evt: PointOwnershipChangeEvt): Game.PointOwnerMapChangeEvt =
    Game.PointOwnerMapChangeEvt.newBuilder().
      setKind(evt match {
      case _: WarpZoneChangeEvt => Game.PointOwnerMapChangeEvt.Kind.WARP_ZONE
      case _: VisibilityChangeEvt => Game.PointOwnerMapChangeEvt.Kind.VISIBILITY
    }).
      addAllOwned(convert(evt.ownedVects)).
      addAllUnowned(convert(evt.unownedVects)).build()

  implicit def convert(evt: WarpEvt): Game.WarpEvt =
    Game.WarpEvt.newBuilder().setObject(convert(evt.obj: WObject)).build()

  implicit def convert(evt: ObjVisibleEvt): Game.ObjVisibleEvt =
    Game.ObjVisibleEvt.newBuilder().setObject(convert(evt.obj)).build()

  implicit def convert(evt: MoveEvt): Game.MoveEvt =
    Game.MoveEvt.newBuilder().setFrom(evt.oldObj.position).setMovesLeft(evt.movesLeft).
      setObjId(evt.oldObj.id).setTo(evt.to).build()

  implicit def convert(evt: AttackEvt[_ <: OwnedObj]): Game.AttackEvt =
    Game.AttackEvt.newBuilder().
      setAttackerId(evt.attacker.id).setDefenderId(evt.defender._1.id).
      setHpLeft(evt.defender._2.map(_.hp).getOrElse(HP(0)).value).
      setAttack(evt.attack).build()

  implicit def convert(evt: MovementChangeEvt): Game.MovementChangeEvt =
    Game.MovementChangeEvt.newBuilder().setObjId(evt.changedObj.id).
      setNewMovement(evt.changedObj.movementLeft).build()

  implicit def convert(evt: ResourceChangeEvt): Game.ResourceChangeEvt =
    Game.ResourceChangeEvt.newBuilder().mapVal { b => evt.obj.fold(
    { case (_, obj) => b.setObjId(obj.id) }, human => b.setPlayerId(human.id)
    )}.setNewResources(evt.newValue.value).build()

  implicit def convert(evt: ActionsChangeEvt): Game.ActionsChangeEvt =
    Game.ActionsChangeEvt.newBuilder().
      setPlayerId(evt.player.id).setNewActions(evt.actions.value).build()

  implicit def convert(evt: HPChangeEvt): Game.HPChangeEvt =
    Game.HPChangeEvt.newBuilder().
      setObjId(evt.newObj.id).
      setNewHp(valWithMax(evt.newObj.hp.value, evt.newObj.maxHp.value)).build()

  implicit def convert(evt: LevelChangeEvt): Game.LevelChangeEvt =
    Game.LevelChangeEvt.newBuilder().
      setObjId(evt.newObj.id).setNewLevel(evt.newObj.level).build()

  implicit def convert(evt: WarpStateChangeEvt): Game.WarpStateChangeEvt =
    Game.WarpStateChangeEvt.newBuilder().
      setObjId(evt.newObj.id).setNewWarpState(evt.newObj.warpState).build()

  implicit def convert(evt: AttacksChangedEvt): Game.AttacksChangeEvt =
    Game.AttacksChangeEvt.newBuilder().
      setObjId(evt.newObj.id).setAttacksLeft(evt.newObj.attacksLeft).build()

  implicit def convert(evt: TurnEndedChangeEvt): Game.TurnEndedChangeEvt =
    Game.TurnEndedChangeEvt.newBuilder().
      setPlayerId(evt.player.id).setNewTurnEnded(evt.turnEnded).build()

  implicit def convert(evt: ObjDestroyedEvt): Game.ObjDestroyedEvt =
    Game.ObjDestroyedEvt.newBuilder().setObjId(evt.obj.id).build()

  implicit def convert(evt: OwnerChangeEvt): Game.OwnerChangeEvt =
    Game.OwnerChangeEvt.newBuilder().setObjId(evt.newObj.id).
      setNewOwnerId(evt.newObj.owner.id).build()

  implicit def convert(evt: ObjectivesUpdatedEvt): Game.ObjectivesUpdateEvt =
    Game.ObjectivesUpdateEvt.newBuilder().setNewObjectives(evt.objectives).build

  implicit def convert(evt: GameWonEvt): Game.GameWonEvt =
    Game.GameWonEvt.newBuilder().setTeamId(evt.team.id).build

  implicit def convert(evt: PopulationChangeEvt): Game.PopulationChangeEvt =
    Game.PopulationChangeEvt.newBuilder().setPlayerId(evt.player.id).
      setNewPopulation(evt.population).build

  implicit def convert(evt: SetTurnTimerEvt): Game.SetTurnTimerEvt =
    Game.SetTurnTimerEvt.newBuilder().setTurnTimeframe(evt.timeframe).build

  implicit def convert(evt: JoinEvt): Game.JoinEvt =
    Game.JoinEvt.newBuilder().setPlayer(convert(evt.human, evt.state)).build()

  implicit def convert(evt: LeaveEvt): Game.LeaveEvt =
    Game.LeaveEvt.newBuilder().setPlayerId(evt.human.id).build()

  implicit def convert(event: FinalEvent): Game.Event =
    Game.Event.newBuilder().mapVal { b => event match {
      case evt: TurnStartedEvt => b.setTurnStarted(evt)
      case evt: TurnEndedEvt => b.setTurnEnded(evt)
      case evt: PointOwnershipChangeEvt => b.setPointOwnerMapChange(evt)
      case evt: WarpEvt => b.setWarp(evt)
      case evt: ObjVisibleEvt => b.setObjVisible(evt)
      case evt: MoveEvt => b.setMove(evt)
      case evt: AttackEvt[_] => b.setAttack(evt)
      case evt: MovementChangeEvt => b.setMovementChange(evt)
      case evt: ResourceChangeEvt => b.setResourceChange(evt)
      case evt: ActionsChangeEvt => b.setActionsChange(evt)
      case evt: HPChangeEvt => b.setHpChange(evt)
      case evt: LevelChangeEvt => b.setLevelChange(evt)
      case evt: JoinEvt => b.setJoin(evt)
      case evt: LeaveEvt => b.setLeave(evt)
      case evt: WarpStateChangeEvt => b.setWarpChange(evt)
      case evt: AttacksChangedEvt => b.setAttacksChange(evt)
      case evt: TurnEndedChangeEvt => b.setTurnEndedChange(evt)
      case evt: ObjDestroyedEvt => b.setObjDestroyed(evt)
      case evt: OwnerChangeEvt => b.setOwnerChange(evt)
      case evt: ObjectivesUpdatedEvt => b.setObjectivesUpdate(evt)
      case evt: GameWonEvt => b.setGameWon(evt)
      case evt: PopulationChangeEvt => b.setPopulationChange(evt)
      case evt: SetTurnTimerEvt => b.setSetTurnTimerEvt(evt)
    } }.build()

  /* Messages */

  implicit def convert(msg: GameActor.Out.Events): Game.MEvents =
    Game.MEvents.newBuilder().addAllEvents(convert(msg.events)).build()

  implicit def convert(msg: GameActor.Out.Error): Game.MError =
    Game.MError.newBuilder().setError(msg.error).build()

  implicit def convert(msg: GameActor.Out.Joined): Game.MJoined =
    Game.MJoined.newBuilder().setPlayer(msg.human).build()

  implicit def convert(msg: GameActor.Out.Movement): Game.MMovement =
    Game.MMovement.newBuilder().setId(msg.id).mapVal { b => msg.response match {
      case m: GameActor.Out.Movement.Movable => b.setPaths(m)
      case m: GameActor.Out.Movement.Immovable => b.setPositions(m)
    } }.build()

  implicit def convert(msg: GameActor.Out.Init): Game.MInit =
    Game.MInit.newBuilder().setBounds(msg.bounds).
      addAllObjects(convert(msg.objects.objects)(convert)).
      addAllWarpZone(convert(msg.warpZonePoints)).
      addAllVisiblePoints(convert(msg.visiblePoints)).
      setSelfTeam(msg.selfTeam).
      addAllOtherTeams(convert(msg.otherTeams)).
      setSelf(msg.self).
      addAllOtherPlayers(convert(msg.others)(convert)).
      setWarpableObjectStats(convertWarpableStats(msg.warpableObjects)).
      addAllAttackMultipliers(msg.attackMultipliers.map { case (from, to) =>
      attackMultiplier(from, to)
    }.asJava).
      setObjectives(msg.objectives).
      mapVal { b => msg.turnTimeframe.fold2(b, b.setTurnTimeframe(_)) }.
      addAllExtractionSpeedRates(convert(msg.extractionSpeeds)(convertInit)).
      build()

  implicit def convert(out: GameActor.ClientOut): Game.FromServer =
    Game.FromServer.newBuilder().mapVal { b => out match {
      case msg: GameActor.Out.Events => b.setEvents(msg)
      case msg: GameActor.Out.Error => b.setError(msg)
      case msg: GameActor.Out.Init => b.setInit(msg)
      case msg: GameActor.Out.Movement => b.setMovement(msg)
    } }.build()

  implicit def convert(
    out: NetClient.Management.Out.CheckNameAvailabilityResponse
    ): Management.CheckNameAvailabilityResponse =
    Management.CheckNameAvailabilityResponse.newBuilder().
      setAvailable(out.available).setName(out.name).build()

  implicit def convert(
    out: NetClient.Management.Out.RegisterResponse
    ): Management.RegisterResponse =
    Management.RegisterResponse.newBuilder().mapVal { b =>
      out.newToken.fold2(b, token => b.setNewSessionToken(token.value))
    }.build()

  implicit def convert(
    out: NetClient.Management.Out.LoginResponse
    ): Management.LoginResponse =
    Management.LoginResponse.newBuilder().mapVal { b => out match {
      case NetClient.Management.Out.InvalidCredentials => b
      case NetClient.Management.Out.LoggedIn(user, token, autogenerated) => b.setData(
        Management.LoginResponse.Data.newBuilder().
          setId(user.id).setUsername(user.name).setSessionToken(token.value).
          setAutogenerated(autogenerated)
      )
    }}.build()

  implicit def convert(
    out: NetClient.Management.Out.GameJoined
    ): Management.GameJoined =
    Management.GameJoined.newBuilder().setPlayer(out.human).build()

  implicit def convert(out: NetClient.Management.Out): Management.FromServer =
    Management.FromServer.newBuilder().mapVal { b => out match {
      case msg: NetClient.Management.Out.CheckNameAvailabilityResponse =>
        b.setCheckNameAvailability(msg)
      case msg: NetClient.Management.Out.RegisterResponse =>
        b.setRegister(msg)
      case msg: NetClient.Management.Out.LoginResponse => b.setLogin(msg)
      case msg: NetClient.Management.Out.GameJoined => b.setGameJoined(msg)
      case NetClient.Management.Out.JoinGameCancelled =>
        b.setGameJoinCancelled(Management.JoinGameCancelled.newBuilder().build())
    } }.build()

  implicit def convert(out: NetClient.Msgs.FromServer.TimeSync)
  : Messages.TimeSync.FromServer =
    Messages.TimeSync.FromServer.newBuilder().
      setClientNow(out.clientNow).setServerNow(out.serverNow).build()

  implicit def convert(out: NetClient.Msgs.FromServer): Messages.FromServer =
    Messages.FromServer.newBuilder().mapVal { b => out match {
      case msg: NetClient.Msgs.FromServer.Game => b.setGame(msg.msg)
      case msg: NetClient.Msgs.FromServer.Management => b.setManagement(msg.msg)
      case msg: NetClient.Msgs.FromServer.TimeSync => b.setTimeSync(msg)
    } }.build()

  def serialize(out: NetClient.Msgs.FromServer): ByteString =
    ByteString(convert(out).toByteArray)
}
