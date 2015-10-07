package app.protobuf.parsing

import app.actors.NetClient.GameInMsg
import app.actors.game.GameActor.In
import app.models.game._
import app.models.game.world._
import app.models.game.world.units._
import app.models.game.world.buildings._
import netmsg._
import implicits._
import scalaz._, Scalaz._
import app.actors.game.GameActor.In._

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

  implicit def parseTarget(o: game.MAttack.Target): String \/ (Vect2 \/ WObject.Id) =
    o match {
      case game.MAttack.Target(Some(pos), _) => -\/(parseVect2(pos)).right
      case game.MAttack.Target(_, Some(id)) => \/-(parseWid(id)).right
      case game.MAttack.Target(None, None) => s"Empty MAttack.Target: $o!".left
    }

  def parseMove(m: game.MMove): String \/ (Human => Move) =
    for (path <- parsePath(m.path)) yield In.Move(_: Human, m.id, path)

  def parse(msg: game.FromClient): String \/ GameInMsg = {
    msg match {
      case game.FromClient(Some(m), _, _, _, _, _, _, _) =>
        \/-(In.Warp(_: Human, m.position, m.warpable))
      case game.FromClient(_, Some(m), _, _, _, _, _, _) =>
        parseMove(m)
      case game.FromClient(_, _, Some(m), _, _, _, _, _) =>
        parseTarget(m.target).map { target =>
          In.Attack(_: Human, m.id, target)
        }
      case game.FromClient(_, _, _, Some(m), _, _, _, _) =>
        \/-(In.Special(_: Human, m.id))
      case game.FromClient(_, _, _, _, Some(m), _, _, _) =>
        for {
          move <- parseMove(m.move)
          target <- parseTarget(m.target)
        } yield move.andThen(In.MoveAttack(_, target))
      case game.FromClient(_, _, _, _, _, Some(m), _, _) =>
        \/-(In.Leave.apply(_: Human))
      case game.FromClient(_, _, _, _, _, _, Some(m), _) =>
        \/-(In.ToggleWaitingForRoundEnd.apply(_: Human))
      case game.FromClient(_, _, _, _, _, _, _, Some(m)) =>
        \/-(In.Concede.apply(_: Human))
      case game.FromClient(None, None, None, None, None, None, None, None) =>
        s"Empty message: $msg!".left
    }
  }
}
