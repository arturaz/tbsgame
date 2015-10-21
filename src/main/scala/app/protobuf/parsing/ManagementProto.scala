package app.protobuf.parsing

import app.actors.NetClient
import app.actors.NetClient.Management.In.JoinGame
import app.actors.NetClient.Management.{SessionToken, PlainPassword, Credentials}
import app.actors.game.GamesManagerActor.BackgroundToken

import netmsg._

import implicits._

import scala.language.implicitConversions
import scalaz._, Scalaz._

trait ManagementProto extends BaseProto {
  implicit def parse(c: management.Credentials): String \/ Credentials =
    c.password.map(PlainPassword.apply)
      .orElse(c.sessionToken.map(SessionToken.apply))
      .map(Credentials(c.name, _))
      .toRightDisjunction(s"No credentials provided in $c!")

  implicit def parse(mode: management.JoinGame.Mode): JoinGame.Mode = {
    if (mode.teams <= 1) JoinGame.Mode.Singleplayer
    else JoinGame.Mode.OneVsOne
  }

  def parse(msg: management.FromClient): String \/ NetClient.Management.In = {
    import management.FromClient
    import app.actors.NetClient.Management.In._

    msg match {
      case FromClient(Some(m), _, _, _, _, _, _) =>
        AutoRegister.right
      case FromClient(_, Some(m), _, _, _, _, _) =>
        CheckNameAvailability(m.name).right
      case FromClient(_, _, Some(m), _, _, _, _) =>
        Register(m.username, PlainPassword(m.password), m.email).right
      case FromClient(_, _, _, Some(m), _, _, _) =>
        for (credentials <- parse(m.credentials)) yield Login(credentials)
      case FromClient(_, _, _, _, Some(m), _, _) =>
        JoinGame(m.mode).right
      case FromClient(_, _, _, _, _, Some(m), _) =>
        CancelJoinGame.right
      case FromClient(_, _, _, _, _, _, Some(m)) =>
        CancelBackgroundToken(BackgroundToken(m.token)).right
      case FromClient(None, None, None, None, None, None, None) =>
        s"Empty msg $msg!".left
    }
  }
}
