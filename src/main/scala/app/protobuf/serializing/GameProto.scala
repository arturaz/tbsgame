package app.protobuf.serializing

import app.models.game._
import app.models.game.events.HumanState
import app.models.game.world._
import app.models.game.world.props.ExtractionSpeed
import netmsg._

import scala.language.implicitConversions

trait GameProto extends BaseProto with GameWObjects with GameEvents {
  /** Ids **/

  implicit def convert(id: WObject.Id): game.WObjID = game.WObjID(id.id)

  implicit def convert(id: Player.Id): game.PlayerID = game.PlayerID(id.id)

  implicit def convert(id: Team.Id): game.TeamID = game.TeamID(id.id)

  implicit def convert(id: World.Id): game.WorldID = game.WorldID(id.id)

  implicit def convert(id: Owner.Id): game.OwnerID = id match {
    case pid: Player.Id => game.OwnerID(playerId = Some(pid))
    case tid: Team.Id => game.OwnerID(teamId = Some(tid))
  }

  /**************************************************************************************/

  implicit def convert(team: Team): game.Team = game.Team(team.id)

  implicit def convert(player: Player): game.Player =
    game.Player(player.asHuman.map(_.name).getOrElse("Bot"), player.id, player.team.id)

  implicit def convert(state: HumanState): game.PlayerState = game.PlayerState(
    actions = state.gameState.actions, population = state.population,
    resources = state.resources, turnEnded = !state.gameState.activity.canAct
  )

  def convert(player: Player, state: Option[HumanState]): game.InitPlayer =
    game.InitPlayer(player = player, state = state.map(convert))

  def convert(t: (Player, Option[HumanState])): game.InitPlayer = convert(t._1, t._2)

  implicit def convert(objectives: RemainingObjectives): game.Objectives =
    game.Objectives(
      gatherResourcesLeft = objectives.gatherResources.map(convert),
      collectVpsLeft = objectives.collectVps.map(convert),
      destroyAllCriticalObjectsLeft = objectives.destroyAllCriticalObjects.map(convert)
    )

  implicit def convert(attack: Attack): game.Attack =
    game.Attack(attackerRoll = attack.attackerRoll, successful = attack.successful)

  implicit def convert(kind: WObjKind): game.WObjKind = kind match {
    case WObjKind.Light => game.WObjKind.LIGHT
    case WObjKind.Medium => game.WObjKind.MEDIUM
    case WObjKind.Armored => game.WObjKind.ARMORED
    case WObjKind.Structure => game.WObjKind.STRUCTURE
  }

  def convertInit(extractionSpeed: ExtractionSpeed): game.MInit.ExtractionSpeedRate =
    game.MInit.ExtractionSpeedRate(
      extractionSpeed = extractionSpeed,
      resourcesPerTurn = extractionSpeed.resourcesPerTurn
    )
}
