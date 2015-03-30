package app.protobuf.parsing

import java.util.UUID

import akka.util.ByteString
import app.actors.NetClient
import app.actors.NetClient.Management.In.JoinGame
import app.actors.NetClient.Management.{SessionToken, PlainPassword, Credentials}
import app.actors.NetClient._
import app.models.game.world.buildings.{LaserTowerStats, WarpLinkerStats, ExtractorStats}
import app.models.game.world.units._
import app.models.game.{Human, Team, Player}
import app.models.game.world.{WarpableCompanion, WObject, Vect2}
import netmsg._
import org.joda.time.DateTime
import utils.data.NonEmptyVector
import collection.JavaConverters._
import implicits._

import scala.language.implicitConversions

object Parsing {
  implicit def parseVect2(v: Base.Vect2): Vect2 = Vect2(v.getX, v.getY)

  implicit def parseUUID(v: Base.UUID): UUID =
    new UUID(v.getMostSignificant, v.getLeastSignificant)

  implicit def parsePid(v: Game.PlayerID): Player.Id = Player.Id(v.getId)
  implicit def parseTid(v: Game.TeamID): Team.Id = Team.Id(v.getId)
  implicit def parseWid(v: Game.WObjID): WObject.Id = WObject.Id(v.getId)

  implicit def parseWarpable(
    w: Game.WarpableKind
  ): WarpableCompanion.Some = w match {
    case Game.WarpableKind.B_EXTRACTOR => ExtractorStats
    case Game.WarpableKind.B_WARP_LINKER => WarpLinkerStats
    case Game.WarpableKind.B_LASER_TOWER => LaserTowerStats
    case Game.WarpableKind.U_CORVETTE => CorvetteStats
    case Game.WarpableKind.U_WASP => WaspStats
    case Game.WarpableKind.U_SCOUT => ScoutStats
    case Game.WarpableKind.U_ROCKET_FRIGATE => RocketFrigateStats
    case Game.WarpableKind.U_RAYSHIP => RayShipStats
    case Game.WarpableKind.U_GUNSHIP => GunshipStats
    case Game.WarpableKind.U_FORTRESS => FortressStats
  }

  def parse(pathList: java.util.List[Base.Vect2]): Vector[Vect2] =
    pathList.asScala.map(parseVect2).toVector

  def parsePath(
    pathList: java.util.List[Base.Vect2]
    ): Either[String, NonEmptyVector[Vect2]] =
    NonEmptyVector.create(parse(pathList)).toRight("Can't create path from empty list!")

  def parse(timestamp: Base.Timestamp): DateTime = new DateTime(timestamp.getTimestamp)

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
    else if (m.hasConcede)
      (Concede.apply(_: Human)).right
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

  implicit def parse(mode: netmsg.Management.JoinGame.Mode): JoinGame.Mode = {
    val teams = mode.getTeams
    if (teams <= 1) JoinGame.Mode.Singleplayer
    else JoinGame.Mode.PvP(teams, mode.getPlayersPerTeam)
  }

  def parse(m: netmsg.Management.FromClient): Either[String, NetClient.Management.In] = {
    import app.actors.NetClient.Management.In._

    if (m.hasAutoRegister)
      AutoRegister.right
    else if (m.hasCheckNameAvailability)
      CheckNameAvailability(m.getCheckNameAvailability.getName).right
    else if (m.hasRegister)
      m.getRegister.mapVal { reg =>
        Register(reg.getUsername, PlainPassword(reg.getPassword), reg.getEmail)
      }.right
    else if (m.hasLogin)
      m.getLogin.getCredentials.mapVal(c => Login(c)).right
    else if (m.hasJoinGame)
      m.getJoinGame.mapVal(jg => JoinGame(jg.getMode)).right
    else if (m.hasCancelJoinGame)
      CancelJoinGame.right
    else
      s"Can't parse $m!".left
  }

  def parse(m: Messages.TimeSync.FromClient): NetClient.Msgs.FromClient.TimeSync =
    NetClient.Msgs.FromClient.TimeSync(parse(m.getNow))

  def parse(m: Messages.FromClient): Either[String, NetClient.Msgs.FromClient] = {
    if (m.hasGame)
      parse(m.getGame).right.map(NetClient.Msgs.FromClient.Game)
    else if (m.hasManagement)
      parse(m.getManagement).right.map(NetClient.Msgs.FromClient.Management)
    else if (m.hasTimeSync)
      parse(m.getTimeSync).right
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
