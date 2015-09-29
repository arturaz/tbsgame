package app.protobuf.parsing

import app.actors.NetClient.GameInMsg
import app.models.game._
import app.models.game.world._
import app.models.game.world.units._
import app.models.game.world.buildings._
import netmsg._
import implicits._

import scala.language.implicitConversions

trait GameProto extends BaseProto {
  implicit def parsePid(v: game.PlayerID): Player.Id = Player.Id(v.id)
  implicit def parseTid(v: game.TeamID): Team.Id = Team.Id(v.id)
  implicit def parseWid(v: game.WObjID): WObject.Id = WObject.Id(v.id)

  implicit def parseWarpable(
    w: game.WarpableKind
  ): WarpableCompanion.Some = w match {
    case game.WarpableKind.B_EXTRACTOR => ExtractorStats
    case game.WarpableKind.B_WARP_LINKER => WarpLinkerStats
    case game.WarpableKind.B_LASER_TOWER => LaserTowerStats
    case game.WarpableKind.B_POPULATION_TOWER => PopulationTowerStats
    case game.WarpableKind.B_ACTION_TOWER => ActionTowerStats
    case game.WarpableKind.U_CORVETTE => CorvetteStats
    case game.WarpableKind.U_WASP => WaspStats
    case game.WarpableKind.U_SCOUT => ScoutStats
    case game.WarpableKind.U_ROCKET_FRIGATE => RocketFrigateStats
    case game.WarpableKind.U_RAYSHIP => RayShipStats
    case game.WarpableKind.U_GUNSHIP => GunshipStats
    case game.WarpableKind.U_FORTRESS => FortressStats
    case game.WarpableKind.U_WARP_PRISM => WarpPrismStats
  }

  def parse(msg: game.FromClient): Either[String, GameInMsg] = {
    import app.actors.game.GameActor.In._

    msg match {
      case game.FromClient(Some(m), _, _, _, _, _, _, _) =>
        Right(Warp(_: Human, m.position, m.warpable))
      case game.FromClient(_, Some(m), _, _, _, _, _, _) =>
        for (path <- parsePath(m.path).right) yield Move(_: Human, m.id, path)
      case game.FromClient(_, _, Some(m), _, _, _, _, _) =>
        Right(Attack(_: Human, id = m.id, targetId = m.targetId))
      case game.FromClient(_, _, _, Some(m), _, _, _, _) =>
        Right(Special(_: Human, m.id))
      case game.FromClient(_, _, _, _, Some(m), _, _, _) =>
        for (path <- parsePath(m.path).right)
          yield MoveAttack(_: Human, id = m.id, path, targetId = m.targetId)
      case game.FromClient(_, _, _, _, _, Some(m), _, _) =>
        Right(Leave.apply(_: Human))
      case game.FromClient(_, _, _, _, _, _, Some(m), _) =>
        Right(EndTurn.apply(_: Human))
      case game.FromClient(_, _, _, _, _, _, _, Some(m)) =>
        Right(Concede.apply(_: Human))
      case game.FromClient(None, None, None, None, None, None, None, None) =>
        s"Empty message: $msg!".left
    }
  }
}
