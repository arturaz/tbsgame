package app.actors

import java.util.UUID

import akka.actor.Actor
import app.actors.game.GameActor
import app.models.world.props.Asteroid
import app.models.{Team, Attack, Player, Human}
import app.models.game.events._
import app.models.world._
import app.models.world.buildings._
import app.models.world.units.{Wasp, Corvette}
import netmsg.{Base, Game}
import implicits._

object NetClient {
  import language.implicitConversions

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

    def parse(m: Game.FromClient): Either[String, GameActor.In] = {
      import GameActor.In._

      if (m.hasJoin) m.getJoin.mapVal { m =>
        Join(Human(m.getName, GameActor.HumanTeam)).right
      }
      else s"Cannot parse $m!".left
    }

    def parse(m: Game.FromClient, human: Human): Either[String, GameActor.In] = {
      import GameActor.In, GameActor.In.{Attack => _, _}

      if (m.hasWarp)
        m.getWarp.mapVal { m => Warp(human, m.getPosition, m.getWarpable)}.right
      else if (m.hasMove)
        m.getMove.mapVal { m => Move(human, m.getId, m.getTarget)}.right
      else if (m.hasAttack)
        m.getAttack.mapVal { m => In.Attack(human, m.getId, m.getTargetId)}.right
      else if (m.hasSpecial)
        m.getSpecial.mapVal { m => Special(human, m.getId)}.right
      else if (m.hasConsumeActions)
        ConsumeActions(human).right
      else if (m.hasLeave)
        Leave(human).right
      else parse(m)
    }
  }

  object Serializing {
    import collection.JavaConverters._

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

    implicit def convert(tileDistance: TileDistance): Int = tileDistance.value
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
        setActionsGiven(obj.companion.actionsGiven).build()

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
          import Game.WObject.Kind._
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
        addAllPositions(convert(evt.positions)).setVisible(evt.visible).build()

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
        setHpLeft(evt.defender._2.map(_.hp).getOrElse(0)).setAttack(evt.attack).build()

    implicit def convert(evt: MovementChangeEvt): Game.MovementChangeEvt =
      Game.MovementChangeEvt.newBuilder().setObjId(evt.changedObj.id).
        setNewMovement(evt.changedObj.movementLeft).build()

    implicit def convert(evt: MovedOrAttackedChangeEvt): Game.MovedOrAttackedChangeEvt =
      Game.MovedOrAttackedChangeEvt.newBuilder().setObjId(evt.changedObj.id).
        setMovedOrAttacked(evt.changedObj.movedOrAttacked).build()

    implicit def convert(evt: ResourceChangeEvt): Game.ResourceChangeEvt =
      Game.ResourceChangeEvt.newBuilder().mapVal { b => evt.obj.fold(
        { case (_, obj) => b.setObjId(obj.id) }, human => b.setPlayerId(human.id)
      )}.setNewResources(evt.newValue).build()

    implicit def convert(evt: ActionsChangeEvt): Game.ActionsChangeEvt =
      Game.ActionsChangeEvt.newBuilder().
        setPlayerId(evt.human.id).setNewActions(evt.actions).build()

    implicit def convert(evt: JoinEvt): Game.JoinEvt =
      Game.JoinEvt.newBuilder().
        setPlayer(evt.human).setActions(evt.state.actions).build()

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
      Game.MInit.newBuilder().setBounds(msg.world.bounds).
        addAllObjects(convert(msg.world.objects)).
        addAllVisiblePoints(convert(msg.world.visibilityMap.map.keys.map(_._1))).
        addAllTeams(convert(msg.world.teams)).
        addAllPlayers(convert(msg.world.players)).
        addAllResources(msg.world.resourcesMap.map { case (player, amount) =>
          Game.Resources.newBuilder().setPlayerId(player.id).setAmount(amount).build()
        }).
        build()

    implicit def convert(out: GameActor.Out): Game.FromServer =
      Game.FromServer.newBuilder().mapVal { b => out match {
        case msg: GameActor.Out.Events => b.setEvents(msg)
        case msg: GameActor.Out.Error => b.setError(msg)
        case msg: GameActor.Out.Joined => b.setJoined(msg)
        case msg: GameActor.Out.Init => b.setInit(msg)
      } }.build()
  }
}

class NetClient extends Actor {
  override def receive = ???
}
