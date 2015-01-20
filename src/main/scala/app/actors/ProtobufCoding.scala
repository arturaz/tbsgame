package app.actors

import java.util.UUID

import akka.util.ByteString
import app.actors.NetClient.GameInMsg
import app.actors.NetClient.Management.{SessionToken, PlainPassword, Credentials}
import app.actors.game.GameActor
import app.algorithms.Pathfinding.Path
import app.models.game.events._
import app.models.game.world._
import app.models.game.world.buildings._
import app.models.game.world.props.Asteroid
import app.models.game.world.units._
import app.models.game._
import implicits._
import netmsg.{Management, Base, Game, Messages}
import utils.{ValWithMax, FloatValueClass, DoubleValueClass, IntValueClass}
import utils.data.NonEmptyVector
import collection.JavaConverters._

object ProtobufCoding {
  import scala.language.implicitConversions

  object Parsing {
    implicit def parseVect2(v: Base.Vect2): Vect2 = Vect2(v.getX, v.getY)

    implicit def parseUUID(v: Base.UUID): UUID =
      new UUID(v.getMostSignificant, v.getLeastSignificant)

    implicit def parsePid(v: Game.PlayerID): Player.Id = Player.Id(v.getId)
    implicit def parseTid(v: Game.TeamID): Team.Id = Team.Id(v.getId)
    implicit def parseWid(v: Game.WObjID): WObject.Id = WObject.Id(v.getId)

    implicit def parseWarpable(
      w: Game.MWarp.HumanWarpable
    ): WarpableCompanion[_ <: Warpable] = w match {
      case Game.MWarp.HumanWarpable.B_EXTRACTOR => Extractor
      case Game.MWarp.HumanWarpable.B_WARP_LINKER => WarpLinker
      case Game.MWarp.HumanWarpable.B_LASER_TOWER => LaserTower
      case Game.MWarp.HumanWarpable.U_CORVETTE => Corvette
      case Game.MWarp.HumanWarpable.U_SCOUT => Scout
      case Game.MWarp.HumanWarpable.U_ROCKET_FRIGATE => RocketFrigate
      case Game.MWarp.HumanWarpable.U_GUNSHIP => Gunship
    }

    def parse(pathList: java.util.List[Base.Vect2]): Vector[Vect2] =
      pathList.asScala.map(parseVect2).toVector

    def parsePath(
      pathList: java.util.List[Base.Vect2]
    ): Either[String, NonEmptyVector[Vect2]] =
      NonEmptyVector.create(parse(pathList)).toRight("Can't create path from empty list!")

    def parse(m: Game.FromClient): Either[String, GameInMsg] = {
      import app.actors.game.GameActor.In
      import app.actors.game.GameActor.In.{Attack => _, _}

      if (m.hasWarp)
        m.getWarp.mapVal { m => Warp(_: Human, m.getPosition, m.getWarpable) }.right
      else if (m.hasMove)
        m.getMove.mapVal { m =>
          parsePath(m.getPathList).right.map(path => Move(_: Human, m.getId, path))
        }
      else if (m.hasAttack)
        m.getAttack.
          mapVal { m => In.Attack(_: Human, m.getId, m.getTargetId) }.right
      else if (m.hasSpecial)
        m.getSpecial.mapVal { m => Special(_: Human, m.getId) }.right
      else if (m.hasEndTurn)
        (EndTurn.apply(_: Human)).right
      else if (m.hasGetMovement)
        m.getGetMovement.mapVal { m => GetMovement(_: Human, m.getId) }.right
      else if (m.hasMoveAttack)
        m.getMoveAttack.mapVal { m =>
          parsePath(m.getPathList).right.map(path => MoveAttack(
            _: Human, m.getId, path, m.getTargetId
          ))
        }
      else if (m.hasLeave)
        (Leave.apply(_: Human)).right
      else
        s"Can't parse $m!".left
    }

    implicit def parse(c: netmsg.Management.Credentials): Credentials = Credentials(
      c.getName,
      if (c.hasPassword) PlainPassword(c.getPassword)
        else SessionToken(c.getSessionToken)
    )

    def parse(m: netmsg.Management.FromClient): Either[String, NetClient.Management.In] = {
      import app.actors.NetClient.Management.In._

      if (m.hasAutoRegister)
        AutoRegister.right
      else if (m.hasCheckNameAvailability)
        CheckNameAvailability(m.getCheckNameAvailability.getName).right
      else if (m.hasChangeCredentials)
        m.getChangeCredentials.mapVal { cc =>
          ChangeCredentials(cc.getUsername, PlainPassword(cc.getPassword))
        }.right
      else if (m.hasLogin)
        m.getLogin.getCredentials.mapVal(c => Login(c)).right
      else if (m.hasJoinGame)
        m.getJoinGame.mapVal(_ => JoinGame).right
      else
        s"Can't parse $m!".left
    }

    def parse(m: Messages.FromClient): Either[String, NetClient.Msgs.FromClient] = {
      if (m.hasGame)
        parse(m.getGame).right.map(NetClient.Msgs.FromClient.Game)
      else if (m.hasManagement)
        parse(m.getManagement).right.map(NetClient.Msgs.FromClient.Management)
      else s"Can't parse $m!".left
    }

    def parse(data: ByteString): Either[String, NetClient.Msgs.FromClient] = {
      try {
        val protoMsg = Messages.FromClient.parseFrom(data.iterator.asInputStream)
        parse(protoMsg)
      }
      catch {
        case e: Throwable => s"Error while parsing protobuf: $e".left
      }
    }
  }

  object Serializing {
    import scala.collection.JavaConverters._

    /* Helpers */

    implicit def convert[A, B]
    (seq: Iterable[A])(implicit converter: A => B): java.lang.Iterable[_ <: B] =
      seq.toIterable.map(converter).asJava

    def valWithMax(current: Int, maximum: Int): Base.ValWithMax =
      Base.ValWithMax.newBuilder().setCurrent(current).setMaximum(maximum).build()

    implicit def convert[A <: IntValueClass[A]](vwm: ValWithMax[A]): Base.ValWithMax =
      Base.ValWithMax.newBuilder().setCurrent(vwm.value).setMaximum(vwm.max).build()

    /* Data */

    implicit def convert(id: UUID): Base.UUID =
      Base.UUID.newBuilder().
        setLeastSignificant(id.getLeastSignificantBits).
        setMostSignificant(id.getMostSignificantBits).build()

    implicit def convert(v: IntValueClass[_]): Int = v.value
    implicit def convert(v: DoubleValueClass[_]): Double = v.value
    implicit def convert(v: FloatValueClass[_]): Float = v.value

    implicit def convert(range: Range.Inclusive): Base.Range =
      Base.Range.newBuilder().setStart(range.start).setEnd(range.end).build()

    implicit def convert(v: Vect2): Base.Vect2 =
      Base.Vect2.newBuilder().setX(v.x).setY(v.y).build()

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
        setResources(state.resources).setTurnEnded(state.gameState.turnEnded).build()

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

    implicit def convert(group: WarpableGroup): Game.WObject.Warpable.Group =
      group match {
        case WarpableGroup.Building => Game.WObject.Warpable.Group.BUILDING
        case WarpableGroup.Unit => Game.WObject.Warpable.Group.UNIT
      }

    implicit def convert(obj: SizedWObjectStats): Game.WObject.SizedObj.Stats =
      Game.WObject.SizedObj.Stats.newBuilder().setSize(obj.size).build()

    implicit def convert(obj: SizedWObject): Game.WObject.SizedObj =
      Game.WObject.SizedObj.newBuilder().setStats(obj.companion).build()

    implicit def convert(obj: OwnedObjStats): Game.WObject.OwnedObj.Stats =
      Game.WObject.OwnedObj.Stats.newBuilder().
        setMaxHp(obj.maxHp).setIsCritical(obj.isCritical).
        setVisibility(obj.visibility).setKind(obj.kind).
        setIsRespawnable(obj.isInstanceOf[RespawnsOnDestructionStats]).
        build()

    implicit def convert(obj: OwnedObj): Game.WObject.OwnedObj =
      Game.WObject.OwnedObj.newBuilder().setStats(obj.companion).
        setHp(obj.hp).setOwnerId(obj.owner.id).build()

    implicit def convert(obj: GivingActionsStats): Game.WObject.GivingActions.Stats =
      Game.WObject.GivingActions.Stats.newBuilder().
        setActionsGiven(obj.actionsGiven.value).build()

    implicit def convert(obj: GivingActions): Game.WObject.GivingActions =
      Game.WObject.GivingActions.newBuilder().setStats(obj.companion).build()

    implicit def convert(obj: GivingPopulationStats): Game.WObject.GivingPopulation.Stats =
      Game.WObject.GivingPopulation.Stats.newBuilder().
        setPopulationGiven(obj.populationGiven.value).build()

    implicit def convert(obj: GivingPopulation): Game.WObject.GivingPopulation =
      Game.WObject.GivingPopulation.newBuilder().setStats(obj.companion).build()

    implicit def convert(obj: WarpableStats): Game.WObject.Warpable.Stats =
      Game.WObject.Warpable.Stats.newBuilder().setCost(obj.cost).
        setWarpTime(obj.warpTime).setPopulationCost(obj.populationCost).
        setGroup(obj.group).build()

    implicit def convert(obj: Warpable): Game.WObject.Warpable =
      Game.WObject.Warpable.newBuilder().setStats(obj.companion).
        setWarpState(obj.warpState).build()

    implicit def convert(obj: SpecialActionStats): Game.WObject.SpecialAction.Stats =
      Game.WObject.SpecialAction.Stats.newBuilder().
        setActionsNeeded(obj.specialActionsNeeded).build()

    implicit def convert(obj: SpecialAction): Game.WObject.SpecialAction =
      Game.WObject.SpecialAction.newBuilder().setStats(obj.companion).build()

    implicit def convert(obj: FighterStats): Game.WObject.Fighter.Stats =
      Game.WObject.Fighter.Stats.newBuilder().
        setAttack(obj.attackDamageRange).setAttackRange(obj.attackRange).
        setAttacks(obj.attacks).
        build()

    implicit def convert(obj: Fighter): Game.WObject.Fighter =
      Game.WObject.Fighter.newBuilder().
        setStats(obj.companion).setAttacksLeft(obj.attacksLeft).setLevel(obj.level).
        build()

    implicit def convert(obj: MoveAttackActionedStats)
    : Game.WObject.MoveAttackActioned.Stats =
      Game.WObject.MoveAttackActioned.Stats.newBuilder().
        setActionsNeeded(obj.moveAttackActionsNeeded).build()

    implicit def convert(obj: MoveAttackActioned): Game.WObject.MoveAttackActioned =
      Game.WObject.MoveAttackActioned.newBuilder().setStats(obj.companion).
        setMovedOrAttacked(obj.movedOrAttacked).build()

    implicit def convert(obj: MovableWObjectStats): Game.WObject.Movable.Stats =
      Game.WObject.Movable.Stats.newBuilder().setMovementRange(obj.movement).build()

    implicit def convert(obj: MovableWObject): Game.WObject.Movable =
      Game.WObject.Movable.newBuilder().setStats(obj.companion).
        setMovement(obj.movementLeft).build()

    implicit def convert(obj: Asteroid): Game.WObject.Asteroid =
      Game.WObject.Asteroid.newBuilder().setResources(obj.resources).build()

    implicit def convert(obj: Extractor.type): Game.WObject.Extractor.Stats =
      Game.WObject.Extractor.Stats.newBuilder().
        setSpecialExtracts(obj.specialExtracts).
        setTurnStartExtracts(obj.turnStartExtracts).
        setSpecialConsumeExtracts(obj.specialCollapseResources).build()

    implicit def convert(obj: Extractor): Game.WObject.Extractor =
      Game.WObject.Extractor.newBuilder().setStats(obj.companion).build()

    implicit def convert(obj: Corvette.type): Game.WObject.Corvette.Stats =
      Game.WObject.Corvette.Stats.newBuilder().
        setSpecialMovementAdded(obj.specialMovementAdded).build()

    implicit def convert(obj: Corvette): Game.WObject.Corvette =
      Game.WObject.Corvette.newBuilder().setStats(obj.companion).build()

    implicit def convert(obj: WObject): Game.WObject =
      Game.WObject.newBuilder().setId(obj.id).setPosition(obj.position).
        mapVal { b => obj.cast[SizedWObject].fold2(b, o => b.setSizedObj(o)) }.
        mapVal { b => obj.cast[OwnedObj].fold2(b, o => b.setOwnedObj(o)) }.
        mapVal { b => obj.cast[GivingActions].fold2(b, o => b.setGivingActions(o)) }.
        mapVal { b => obj.cast[GivingPopulation].fold2(b, o => b.setGivingPopulation(o)) }.
        mapVal { b => obj.cast[Warpable].fold2(b, o => b.setWarpable(o)) }.
        mapVal { b => obj.cast[SpecialAction].fold2(b, o => b.setSpecialAction(o)) }.
        mapVal { b => obj.cast[Fighter].fold2(b, o => b.setFighter(o)) }.
        mapVal { b =>
          obj.cast[MoveAttackActioned].fold2(b, o => b.setMoveAttackActioned(o))
        }.
        mapVal { b => obj.cast[MovableWObject].fold2(b, o => b.setMovable(o)) }.
        mapVal { b =>
          import netmsg.Game.WObject.Kind._
          obj match {
            case o: Asteroid => b.setKind(P_ASTEROID).setAsteroid(o)
            case o: WarpGate => b.setKind(B_WARP_GATE)
            case o: Extractor => b.setKind(B_EXTRACTOR).setExtractor(o)
            case o: WarpLinker => b.setKind(B_WARP_LINKER)
            case o: Spawner => b.setKind(B_SPAWNER)
            case o: LaserTower => b.setKind(B_LASER_TOWER)
            case o: Corvette => b.setKind(U_CORVETTE).setCorvette(o)
            case o: Wasp => b.setKind(U_WASP)
            case o: Scout => b.setKind(U_SCOUT)
            case o: RayShip => b.setKind(U_RAY_SHIP)
            case o: RocketFrigate => b.setKind(U_ROCKET_FRIGATE)
            case o: Gunship => b.setKind(U_GUNSHIP)
            case o: Fortress => b.setKind(U_FORTRESS)
            case o: VPTower => b.setKind(B_VP_TOWER)
          }
        }.build()

    implicit def convert(obj: WObjectStats): Game.WObject.Stats =
      Game.WObject.Stats.newBuilder().
        mapVal { b => obj.cast[SizedWObjectStats].fold2(b, o => b.setSizedObj(o)) }.
        mapVal { b => obj.cast[OwnedObjStats].fold2(b, o => b.setOwnedObj(o)) }.
        mapVal { b => obj.cast[GivingActionsStats].fold2(b, o => b.setGivingActions(o)) }.
        mapVal { b => obj.cast[GivingPopulationStats].fold2(b, o => b.setGivingPopulation(o)) }.
        mapVal { b => obj.cast[WarpableStats].fold2(b, o => b.setWarpable(o)) }.
        mapVal { b => obj.cast[SpecialActionStats].fold2(b, o => b.setSpecialAction(o)) }.
        mapVal { b => obj.cast[FighterStats].fold2(b, o => b.setFighter(o)) }.
        mapVal { b =>
          obj.cast[MoveAttackActionedStats].fold2(b, o => b.setMoveAttackActioned(o))
        }.
        mapVal { b => obj.cast[MovableWObjectStats].fold2(b, o => b.setMovable(o)) }.
        mapVal { b =>
          import netmsg.Game.WObject.Kind._
          obj match {
            case Asteroid => b.setKind(P_ASTEROID)
            case WarpGate => b.setKind(B_WARP_GATE)
            case o: Extractor.type => b.setKind(B_EXTRACTOR).setExtractor(o)
            case WarpLinker => b.setKind(B_WARP_LINKER)
            case Spawner => b.setKind(B_SPAWNER)
            case LaserTower => b.setKind(B_LASER_TOWER)
            case o: Corvette.type => b.setKind(U_CORVETTE).setCorvette(o)
            case Wasp => b.setKind(U_WASP)
            case Scout => b.setKind(U_SCOUT)
            case RayShip => b.setKind(U_RAY_SHIP)
            case RocketFrigate => b.setKind(U_ROCKET_FRIGATE)
            case Gunship => b.setKind(U_GUNSHIP)
            case Fortress => b.setKind(U_FORTRESS)
          }
        }.build()

    implicit def convert(obj: GameActor.Out.Init.Stats): Game.InitWObjectStats =
      Game.InitWObjectStats.newBuilder().mapVal { b =>
        if (obj.showInWarpables) b.setWarpable {
          import Game.MWarp.HumanWarpable._
          obj.stats match {
            case Extractor => B_EXTRACTOR
            case LaserTower => B_LASER_TOWER
            case WarpLinker => B_WARP_LINKER
            case Corvette => U_CORVETTE
            case Scout => U_SCOUT
            case RocketFrigate => U_ROCKET_FRIGATE
            case Gunship => U_GUNSHIP
          }
        }
        else b
      }.
      setStats(obj.stats).build()

    implicit def convert(attack: Attack): Game.Attack =
      Game.Attack.newBuilder().setAttackerRoll(attack.attackerRoll).
        setSuccessful(attack.successful).build()

    implicit def convert(kind: WObjKind): Game.WObjKind = kind match {
      case WObjKind.Light => Game.WObjKind.LIGHT
      case WObjKind.Medium => Game.WObjKind.MEDIUM
      case WObjKind.Heavy => Game.WObjKind.HEAVY
    }

    def attackMultiplier(from: WObjKind, to: WObjKind): Game.MInit.AttackMultiplier =
      Game.MInit.AttackMultiplier.newBuilder().
        setFromKind(from).setToKind(to).setMultiplier(from.multiplierAt(to).toFloat).
        build()

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
      Game.WarpEvt.newBuilder().setObject(evt.obj).build()

    implicit def convert(evt: ObjVisibleEvt): Game.ObjVisibleEvt =
      Game.ObjVisibleEvt.newBuilder().setObject(evt.obj).build()

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

    implicit def convert(evt: MovedOrAttackedChangeEvt): Game.MovedOrAttackedChangeEvt =
      Game.MovedOrAttackedChangeEvt.newBuilder().setObjId(evt.changedObj.id).
        setMovedOrAttacked(evt.changedObj.movedOrAttacked).build()

    implicit def convert(evt: ResourceChangeEvt): Game.ResourceChangeEvt =
      Game.ResourceChangeEvt.newBuilder().mapVal { b => evt.obj.fold(
      { case (_, obj) => b.setObjId(obj.id) }, human => b.setPlayerId(human.id)
      )}.setNewResources(evt.newValue.value).build()

    implicit def convert(evt: ActionsChangeEvt): Game.ActionsChangeEvt =
      Game.ActionsChangeEvt.newBuilder().
        setPlayerId(evt.human.id).setNewActions(evt.actions.value).build()

    implicit def convert(evt: HPChangeEvt): Game.HPChangeEvt =
      Game.HPChangeEvt.newBuilder().
        setObjId(evt.newObj.id).setNewHp(evt.newObj.hp).build()

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
        setPlayerId(evt.human.id).setNewTurnEnded(evt.turnEnded).build()

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
        case evt: MovedOrAttackedChangeEvt => b.setMovedOrAttackedChange(evt)
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
        addAllObjects(convert(msg.objects.objects)).
        addAllWarpZone(convert(msg.warpZonePoints)).
        addAllVisiblePoints(convert(msg.visiblePoints)).
        setSelfTeam(msg.selfTeam).
        addAllOtherTeams(convert(msg.otherTeams)).
        setSelf(msg.self).
        addAllOtherPlayers(convert(msg.others)(convert)).
        addAllWobjectStats(convert(msg.wObjectStats)).
        addAllAttackMultipliers(msg.attackMultipliers.map { case (from, to) =>
          attackMultiplier(from, to)
        }.asJava).
        setObjectives(msg.objectives).
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
      out: NetClient.Management.Out.ChangeCredentialsResponse
    ): Management.ChangeCredentialsResponse =
      Management.ChangeCredentialsResponse.newBuilder().mapVal { b =>
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
        case msg: NetClient.Management.Out.ChangeCredentialsResponse =>
          b.setChangeCredentials(msg)
        case msg: NetClient.Management.Out.LoginResponse => b.setLogin(msg)
        case msg: NetClient.Management.Out.GameJoined => b.setGameJoined(msg)
      } }.build()

    implicit def convert(out: NetClient.Msgs.FromServer): Messages.FromServer =
      Messages.FromServer.newBuilder().mapVal { b => out match {
        case msg: NetClient.Msgs.FromServer.Game => b.setGame(msg.msg)
        case msg: NetClient.Msgs.FromServer.Management => b.setManagement(msg.msg)
      } }.build()

    def serialize(out: NetClient.Msgs.FromServer): ByteString =
      ByteString(convert(out).toByteArray)
  }
}
