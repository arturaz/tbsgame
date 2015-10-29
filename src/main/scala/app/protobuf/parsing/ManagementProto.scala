package app.protobuf.parsing

import app.actors.NetClient
import app.actors.NetClient.LoggedInState.JoinGame
import app.actors.game.GamesManagerActor.BackgroundToken
import app.actors.net_client.{Credentials, SessionToken, PlainPassword}

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

  def parse(msg: management.FromClient): String \/ NetClient.ManagementIn = {
    import management.FromClient
    import app.actors.NetClient._

    msg match {
      case FromClient(Some(m), _, _, _, _, _, _, _) =>
        NotLoggedInState.AutoRegister.right
      case FromClient(_, Some(m), _, _, _, _, _, _) =>
        LoggedInState.CheckNameAvailability(m.name).right
      case FromClient(_, _, Some(m), _, _, _, _, _) =>
        LoggedInState.Register(m.username, PlainPassword(m.password), m.email).right
      case FromClient(_, _, _, Some(m), _, _, _, _) =>
        for (credentials <- parse(m.credentials)) yield NotLoggedInState.Login(credentials)
      case FromClient(_, _, _, _, Some(m), _, _, _) =>
        LoggedInState.JoinGame(m.mode).right
      case FromClient(_, _, _, _, _, Some(m), _, _) =>
        LoggedInState.CancelJoinGame.right
      case FromClient(_, _, _, _, _, _, Some(m), _) =>
        NotLoggedInState.CancelBackgroundToken(BackgroundToken(m.token)).right
      case FromClient(_, _, _, _, _, _, _, Some(m)) =>
        LoggedInState.GoingToBackground.right
      case FromClient(None, None, None, None, None, None, None, None) =>
        "Empty msg management.FromClient!".left
    }
  }
}
