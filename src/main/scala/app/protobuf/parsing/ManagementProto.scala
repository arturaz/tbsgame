package app.protobuf.parsing

import app.actors.NetClient
import app.actors.NetClient.Management.In.JoinGame
import app.actors.NetClient.Management.{SessionToken, PlainPassword, Credentials}

import netmsg._

import implicits._

import scala.language.implicitConversions

trait ManagementProto extends BaseProto {
  implicit def parse(c: management.Credentials): Either[String, Credentials] =
    c.password.map(PlainPassword.apply)
      .orElse(c.sessionToken.map(SessionToken.apply))
      .map(Credentials(c.name, _))
      .toRight(s"No credentials provided in $c!")

  implicit def parse(mode: management.JoinGame.Mode): JoinGame.Mode = {
    if (mode.teams <= 1) JoinGame.Mode.Singleplayer
    else JoinGame.Mode.PvP(teams = mode.teams, playersPerTeam = mode.playersPerTeam)
  }

  def parse(msg: management.FromClient): Either[String, NetClient.Management.In] = {
    import management.FromClient
    import app.actors.NetClient.Management.In._

    msg match {
      case FromClient(Some(m), _, _, _, _, _) =>
        AutoRegister.right
      case FromClient(_, Some(m), _, _, _, _) =>
        CheckNameAvailability(m.name).right
      case FromClient(_, _, Some(m), _, _, _) =>
        Register(m.username, PlainPassword(m.password), m.email).right
      case FromClient(_, _, _, Some(m), _, _) =>
        for (credentials <- parse(m.credentials).right) yield Login(credentials)
      case FromClient(_, _, _, _, Some(m), _) =>
        JoinGame(m.mode).right
      case FromClient(_, _, _, _, _, Some(m)) =>
        CancelJoinGame.right
      case FromClient(None, None, None, None, None, None) =>
        s"Empty msg $msg!".left
    }
  }
}
