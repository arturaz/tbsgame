package app.protobuf.serializing

import app.models.game.events._
import app.models.game.world.{HP, OwnedObj}
import netmsg._
import implicits._

import scala.language.implicitConversions

trait GameEvents { _: BaseProto with GameProto with GameWObjects =>
  implicit def convert(evt: RoundStartedEvt.type): game.RoundStartedEvt =
    game.RoundStartedEvt()

  implicit def convert(evt: TurnStartedEvt): game.TurnStartedEvt =
    game.TurnStartedEvt(evt.player.id, evt.timeframe.map(convert))

  implicit def convert(evt: PointOwnershipChangeEvt): game.PointOwnerMapChangeEvt =
    game.PointOwnerMapChangeEvt(
      kind = evt match {
        case _: WarpZoneChangeEvt => game.PointOwnerMapChangeEvt.Kind.WARP_ZONE
        case _: VisibilityChangeEvt => game.PointOwnerMapChangeEvt.Kind.VISIBILITY
      },
      owned = convertSeq(evt.ownedVects), unowned = convertSeq(evt.unownedVects)
    )

  implicit def convert(evt: WarpEvt): game.WarpEvt =
    game.WarpEvt(evt.obj)

  implicit def convert(evt: ObjVisibleEvt): game.ObjVisibleEvt =
    game.ObjVisibleEvt(convert(evt.obj))

  implicit def convert(evt: MoveEvt): game.MoveEvt =
    game.MoveEvt(
      from = evt.oldObj.position, to = evt.to, movesLeft = evt.movesLeft,
      objId = evt.oldObj.id
    )

  implicit def convert(evt: AttackEvt[_ <: OwnedObj]): game.AttackEvt =
    game.AttackEvt(
      attackerId = evt.attacker.id, defenderId = evt.defender._1.id,
      hpLeft = evt.defender._2.map(_.hp).getOrElse(HP(0)).value,
      attack = evt.attack
    )

  implicit def convert(evt: MovementChangeEvt): game.MovementChangeEvt =
    game.MovementChangeEvt(evt.changedObj.id, evt.changedObj.movementLeft)

  implicit def convert(evt: ResourceChangeEvt): game.ResourceChangeEvt =
    game.ResourceChangeEvt(newResources = evt.newValue.value).mapVal { b => evt.obj.fold(
      { case (_, obj) => b.withObjId(obj.id) }, human => b.withPlayerId(human.id)
    ) }

  implicit def convert(evt: ActionsChangeEvt): game.ActionsChangeEvt =
    game.ActionsChangeEvt(evt.player.id, evt.actions.value)

  implicit def convert(evt: HPChangeEvt): game.HPChangeEvt =
    game.HPChangeEvt(
      evt.newObj.id, valWithMax(evt.newObj.hp.value, evt.newObj.maxHp.value)
    )

  implicit def convert(evt: LevelChangeEvt): game.LevelChangeEvt =
    game.LevelChangeEvt(evt.newObj.id, evt.newObj.level)

  implicit def convert(evt: WarpStateChangeEvt): game.WarpStateChangeEvt =
    game.WarpStateChangeEvt(evt.newObj.id, evt.newObj.warpState)

  implicit def convert(evt: AttacksChangedEvt): game.AttacksChangeEvt =
    game.AttacksChangeEvt(evt.newObj.id, evt.newObj.attacksLeft)

  implicit def convert(evt: TurnEndedChangeEvt): game.TurnEndedChangeEvt =
    game.TurnEndedChangeEvt(evt.player.id, evt.turnEnded)

  implicit def convert(evt: ObjDestroyedEvt): game.ObjDestroyedEvt =
    game.ObjDestroyedEvt(evt.obj.id)

  implicit def convert(evt: OwnerChangeEvt): game.OwnerChangeEvt =
    game.OwnerChangeEvt(evt.newObj.id, evt.newObj.owner.id)

  implicit def convert(evt: ObjectivesUpdatedEvt): game.ObjectivesUpdateEvt =
    game.ObjectivesUpdateEvt(evt.objectives)

  implicit def convert(evt: GameWonEvt): game.GameWonEvt =
    game.GameWonEvt(evt.team.id)

  implicit def convert(evt: PopulationChangeEvt): game.PopulationChangeEvt =
    game.PopulationChangeEvt(evt.player.id, evt.population)

  implicit def convert(evt: JoinEvt): game.JoinEvt =
    game.JoinEvt(convert(evt.human, evt.state))

  implicit def convert(evt: LeaveEvt): game.LeaveEvt =
    game.LeaveEvt(evt.human.id)

  implicit def convert(event: FinalEvent): game.Event =
    event match {
      case evt: RoundStartedEvt.type => game.Event(roundStarted = Some(evt))
      case evt: TurnStartedEvt => game.Event(turnStarted = Some(evt))
      case evt: PointOwnershipChangeEvt => game.Event(pointOwnerMapChange = Some(evt))
      case evt: WarpEvt => game.Event(warp = Some(evt))
      case evt: ObjVisibleEvt => game.Event(objVisible = Some(evt))
      case evt: MoveEvt => game.Event(move = Some(evt))
      case evt: AttackEvt[_] => game.Event(attack = Some(evt))
      case evt: MovementChangeEvt => game.Event(movementChange = Some(evt))
      case evt: ResourceChangeEvt => game.Event(resourceChange = Some(evt))
      case evt: ActionsChangeEvt => game.Event(actionsChange = Some(evt))
      case evt: HPChangeEvt => game.Event(hpChange = Some(evt))
      case evt: LevelChangeEvt => game.Event(levelChange = Some(evt))
      case evt: JoinEvt => game.Event(join = Some(evt))
      case evt: LeaveEvt => game.Event(leave = Some(evt))
      case evt: WarpStateChangeEvt => game.Event(warpChange = Some(evt))
      case evt: AttacksChangedEvt => game.Event(attacksChange = Some(evt))
      case evt: TurnEndedChangeEvt => game.Event(turnEndedChange = Some(evt))
      case evt: ObjDestroyedEvt => game.Event(objDestroyed = Some(evt))
      case evt: OwnerChangeEvt => game.Event(ownerChange = Some(evt))
      case evt: ObjectivesUpdatedEvt => game.Event(objectivesUpdate = Some(evt))
      case evt: GameWonEvt => game.Event(gameWon = Some(evt))
      case evt: PopulationChangeEvt => game.Event(populationChange = Some(evt))
    }
}
