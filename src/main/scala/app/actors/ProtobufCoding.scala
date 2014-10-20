package app.actors

import java.util.UUID

import akka.util.ByteString
import app.actors.NetClient.GameInMsg
import app.actors.game.GameActor
import app.models.game.events._
import app.models.game.world._
import app.models.game.world.buildings._
import app.models.game.world.props.Asteroid
import app.models.game.world.units.{Corvette, Wasp}
import app.models.game._
import implicits._
import netmsg.{Management, Base, Game, Messages}
import utils.IntValueClass

object ProtobufCoding {
  import scala.language.implicitConversions

  object Parsing {
    implicit def parseVect2(v: Base.Vect2) = Vect2(v.getX, v.getY)

    implicit def parseUUID(v: Base.UUID) =
      new UUID(v.getMostSignificant, v.getLeastSignificant)

    implicit def parseWarpable(
      w: Game.MWarp.HumanWarpable
      ): WarpableCompanion[_ <: Warpable] = w match {
      case Game.MWarp.HumanWarpable.B_EXTRACTOR => Extractor
      case Game.MWarp.HumanWarpable.B_WARP_LINKER => WarpLinker
      case Game.MWarp.HumanWarpable.B_LASER_TOWER => LaserTower
      case Game.MWarp.HumanWarpable.U_CORVETTE => Corvette
    }

    def parse(m: Game.FromClient): Either[String, GameInMsg] = {
      import app.actors.game.GameActor.In
      import app.actors.game.GameActor.In.{Attack => _, _}

      if (m.hasWarp)
        m.getWarp.mapVal { m => Warp(_: Human, m.getPosition, m.getWarpable) }.right
      else if (m.hasMove)
        m.getMove.mapVal { m => Move(_: Human, m.getId, m.getTarget) }.right
      else if (m.hasAttack)
        m.getAttack.mapVal { m => In.Attack(_: Human, m.getId, m.getTargetId) }.right
      else if (m.hasSpecial)
        m.getSpecial.mapVal { m => Special(_: Human, m.getId) }.right
      else if (m.hasConsumeActions)
        (ConsumeActions.apply(_: Human)).right
      else if (m.hasLeave)
        (Leave.apply(_: Human)).right
      else
        s"Can't parse $m!".left
    }

    def parse(m: netmsg.Management.FromClient): Either[String, NetClient.Management.In] = {
      import app.actors.NetClient.Management.In._

      if (m.hasLogin)
        m.getLogin.mapVal(m => Login(m.getName, m.getPassword)).right
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

    /* Data */

    implicit def convert(id: WObject.Id): Base.UUID =
      Base.UUID.newBuilder().
        setLeastSignificant(id.getLeastSignificantBits).
        setMostSignificant(id.getMostSignificantBits).build()

    implicit def convert(v: IntValueClass[_]): Int = v.value

    implicit def convert(range: Range.Inclusive): Base.Range =
      Base.Range.newBuilder().setStart(range.start).setEnd(range.end).build()

    implicit def convert(v: Vect2): Base.Vect2 =
      Base.Vect2.newBuilder().setX(v.x).setY(v.y).build()

    implicit def convert(b: Bounds): Base.Bounds =
      Base.Bounds.newBuilder().setStart(b.start).setEnd(b.end).build()

    implicit def convert(player: Player): Game.Player =
      Game.Player.newBuilder().
        setId(player.id).setName(player.asHuman.map(_.name).getOrElse("Bot")).
        setTeamId(player.team.id).build()

    implicit def convert(state: HumanState): Game.PlayerState =
      Game.PlayerState.newBuilder().
        setActions(state.gameState.actions).
        setResources(state.resources).build()

    def convert(player: Player, state: Option[HumanState]): Game.InitPlayer =
      Game.InitPlayer.newBuilder().setPlayer(convert(player)).
        mapVal { b => state.fold2(b, b.setState(_)) }.build()

    def convert(t: (Player, Option[HumanState])): Game.InitPlayer = convert(t._1, t._2)

    implicit def convert(team: Team): Game.Team =
      Game.Team.newBuilder().setId(team.id).build()

    implicit def convert(obj: SizedWObject): Game.WObject.SizedObj =
      Game.WObject.SizedObj.newBuilder().setSize(obj.companion.size).build()

    implicit def convert(obj: OwnedObj): Game.WObject.OwnedObj =
      Game.WObject.OwnedObj.newBuilder().setDefense(obj.companion.defense).
        setHp(valWithMax(obj.hp, obj.companion.maxHp)).
        setIsCritical(obj.companion.isCritical).setOwnerId(obj.owner.id).
        setVisibility(obj.companion.visibility).build()

    implicit def convert(obj: GivingActions): Game.WObject.GivingActions =
      Game.WObject.GivingActions.newBuilder().
        setActionsGiven(obj.companion.actionsGiven.value).build()

    implicit def convert(obj: Warpable): Game.WObject.Warpable =
      Game.WObject.Warpable.newBuilder().setCost(obj.companion.cost).
        setWarpState(valWithMax(obj.warpState, obj.companion.warpTime)).build()

    implicit def convert(obj: SpecialAction): Game.WObject.SpecialAction =
      Game.WObject.SpecialAction.newBuilder().
        setActionsNeeded(obj.companion.specialActionsNeeded).build()

    implicit def convert(obj: Fighter): Game.WObject.Fighter =
      Game.WObject.Fighter.newBuilder().
        setAttack(obj.companion.attack).setAttacked(obj.hasAttacked).
        setAttackRange(obj.companion.attackRange).build()

    implicit def convert(obj: MoveAttackActioned): Game.WObject.MoveAttackActioned =
      Game.WObject.MoveAttackActioned.newBuilder().
        setActionsNeeded(obj.companion.moveAttackActionsNeeded).
        setMovedOrAttacked(obj.movedOrAttacked).build()

    implicit def convert(obj: MovableWObject): Game.WObject.Movable =
      Game.WObject.Movable.newBuilder().setMovement(obj.movementLeft).build()

    implicit def convert(obj: Asteroid): Game.WObject.Asteroid =
      Game.WObject.Asteroid.newBuilder().setResources(obj.resources).build()

    implicit def convert(obj: Extractor): Game.WObject.Extractor =
      Game.WObject.Extractor.newBuilder().
        setSpecialExtracts(obj.companion.specialExtracts).
        setTurnStartExtracts(obj.companion.turnStartExtracts).build()

    implicit def convert(obj: Corvette): Game.WObject.Corvette =
      Game.WObject.Corvette.newBuilder().
        setSpecialMovementAdded(obj.companion.specialMovementAdded).build()

    implicit def convert(obj: WObject): Game.WObject =
      Game.WObject.newBuilder().setId(obj.id).setPosition(obj.position).
        mapVal { b => obj.cast[SizedWObject].fold2(b, o => b.setSizedObj(o)) }.
        mapVal { b => obj.cast[OwnedObj].fold2(b, o => b.setOwnedObj(o)) }.
        mapVal { b => obj.cast[GivingActions].fold2(b, o => b.setGivingActions(o)) }.
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
        }
      }.build()

    implicit def convert(attack: Attack): Game.Attack =
      Game.Attack.newBuilder().setAttackerRoll(attack.attackerRoll).
        setDefenderRoll(attack.defenderRoll).setSuccessful(attack.successful).build()

    /* Events */

    implicit def convert(evt: TurnStartedEvt): Game.TurnStartedEvt =
      Game.TurnStartedEvt.newBuilder().setTeamId(evt.team.id).build()

    implicit def convert(evt: TurnEndedEvt): Game.TurnEndedEvt =
      Game.TurnEndedEvt.newBuilder().setTeamId(evt.team.id).build()

    implicit def convert(evt: VisibilityChangeEvt): Game.VisibilityChangeEvt =
      Game.VisibilityChangeEvt.newBuilder().
        addAllVisiblePositions(convert(evt.visiblePositions)).
        addAllInvisiblePositions(convert(evt.invisiblePositions)).build()

    implicit def convert(evt: WarpEvt): Game.WarpEvt =
      Game.WarpEvt.newBuilder().setObject(evt.obj).build()

    implicit def convert(evt: ObjVisibleEvt): Game.ObjVisibleEvt =
      Game.ObjVisibleEvt.newBuilder().setObject(evt.obj).build()

    implicit def convert(evt: MoveEvt): Game.MoveEvt =
      Game.MoveEvt.newBuilder().setFrom(evt.oldObj.position).setMovesLeft(evt.movesLeft).
        setObjId(evt.oldObj.id).setTo(evt.to).build()

    implicit def convert(evt: AttackEvt[_ <: OwnedObj]): Game.AttackEvt =
      Game.AttackEvt.newBuilder().
        setAttackerId(evt.attacker.id).setAttackerPosition(evt.attacker.position).
        setDefenderId(evt.defender._1.id).
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

    implicit def convert(evt: JoinEvt): Game.JoinEvt =
      Game.JoinEvt.newBuilder().setPlayer(convert(evt.human, Some(evt.state))).build()

    implicit def convert(evt: LeaveEvt): Game.LeaveEvt =
      Game.LeaveEvt.newBuilder().setPlayerId(evt.human.id).build()

    implicit def convert(event: Event): Game.Event =
      Game.Event.newBuilder().mapVal { b => event match {
        case evt: TurnStartedEvt => b.setTurnStarted(evt)
        case evt: TurnEndedEvt => b.setTurnEnded(evt)
        case evt: VisibilityChangeEvt => b.setVisibilityChange(evt)
        case evt: WarpEvt => b.setWarp(evt)
        case evt: ObjVisibleEvt => b.setObjVisible(evt)
        case evt: MoveEvt => b.setMove(evt)
        case evt: AttackEvt[_] => b.setAttack(evt)
        case evt: MovementChangeEvt => b.setMovementChange(evt)
        case evt: MovedOrAttackedChangeEvt => b.setMovedOrAttackedChange(evt)
        case evt: ResourceChangeEvt => b.setResourceChange(evt)
        case evt: ActionsChangeEvt => b.setActionsChange(evt)
        case evt: JoinEvt => b.setJoin(evt)
        case evt: LeaveEvt => b.setLeave(evt)
      } }.build()

    /* Messages */

    implicit def convert(msg: GameActor.Out.Events): Game.MEvents =
      Game.MEvents.newBuilder().addAllEvents(convert(msg.events)).build()

    implicit def convert(msg: GameActor.Out.Error): Game.MError =
      Game.MError.newBuilder().setError(msg.error).build()

    implicit def convert(msg: GameActor.Out.Joined): Game.MJoined =
      Game.MJoined.newBuilder().setPlayer(msg.human).build()

    implicit def convert(msg: GameActor.Out.Init): Game.MInit =
      Game.MInit.newBuilder().setBounds(msg.bounds).
        addAllObjects(convert(msg.objects)).
        addAllVisiblePoints(convert(msg.visiblePoints)).
        setSelfTeam(msg.selfTeam).
        addAllOtherTeams(convert(msg.otherTeams)).
        setSelf(msg.self).
        addAllOtherPlayers(convert(msg.others)(convert)).
        build()

    implicit def convert(out: GameActor.ClientOut): Game.FromServer =
      Game.FromServer.newBuilder().mapVal { b => out match {
        case msg: GameActor.Out.Events => b.setEvents(msg)
        case msg: GameActor.Out.Error => b.setError(msg)
        case msg: GameActor.Out.Init => b.setInit(msg)
      } }.build()

    implicit def convert(
      out: NetClient.Management.Out.LoginResponse
    ): Management.LoginResponse =
      Management.LoginResponse.newBuilder().mapVal { b => out match {
        case NetClient.Management.Out.InvalidCredentials => b
        case NetClient.Management.Out.LoggedIn(id) => b.setId(id)
      }}.build()

    implicit def convert(
      out: NetClient.Management.Out.GameJoined
    ): Management.GameJoined =
      Management.GameJoined.newBuilder().setPlayer(out.human).build()

    implicit def convert(out: NetClient.Management.Out): Management.FromServer =
      Management.FromServer.newBuilder().mapVal { b => out match {
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
