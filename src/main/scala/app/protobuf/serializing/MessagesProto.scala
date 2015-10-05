package app.protobuf.serializing

import akka.util.ByteString
import app.actors.NetClient
import app.actors.game.GameActor
import netmsg._
import implicits._
import scalaz._, Scalaz._

import scala.language.implicitConversions

trait MessagesProto extends Helpers { _: GameProto =>
  implicit def convert(msg: GameActor.Out.Events): game.MEvents =
    game.MEvents(convertSeq(msg.events))

  implicit def convert(msg: GameActor.Out.Error): game.MError =
    game.MError(msg.error)

  implicit def convert(msg: GameActor.Out.Joined): game.MJoined =
    game.MJoined(msg.human)

  implicit def convert(msg: GameActor.Out.Init): game.MInit =
    game.MInit(
      id = msg.id, bounds = msg.bounds,
      objects = convertSeq(msg.objects.objects),
      warpZone = convertSeq(msg.warpZonePoints),
      visiblePoints = convertSeq(msg.visiblePoints),
      selfTeam = msg.selfTeam, otherTeams = convertSeq(msg.otherTeams),
      self = msg.self, otherPlayers = convertSeq(msg.others)(convert),
      warpableObjectStats = convertWarpableStats(msg.warpableObjects),
      attackMultipliers = msg.attackMultipliers.map { case (from, to) =>
        attackMultiplier(from, to)
      }(collection.breakOut),
      objectives = msg.objectives,
      turnStarted = msg.currentTurn,
      extractionSpeedRates = convertSeq(msg.extractionSpeeds)(convertInit)
    )

  implicit def convert(out: GameActor.ClientOut): game.FromServer =
    out match {
      case msg: GameActor.Out.Events => game.FromServer(events = Some(msg))
      case msg: GameActor.Out.Error => game.FromServer(error = Some(msg))
      case msg: GameActor.Out.Init => game.FromServer(init = Some(msg))
    }

  implicit def convert(
    out: NetClient.Management.Out.CheckNameAvailabilityResponse
  ): management.CheckNameAvailabilityResponse =
    management.CheckNameAvailabilityResponse(out.name, out.available)

  implicit def convert(
    out: NetClient.Management.Out.RegisterResponse
  ): management.RegisterResponse =
    management.RegisterResponse(out.newToken.map(_.value))

  implicit def convert(
    out: NetClient.Management.Out.LoginResponse
  ): management.LoginResponse =
    out match {
      case NetClient.Management.Out.InvalidCredentials => management.LoginResponse()
      case NetClient.Management.Out.LoggedIn(user, token, autogenerated) =>
        management.LoginResponse(Some(management.LoginResponse.Data(
          id = user.id, username = user.name, sessionToken = token.value,
          autogenerated = autogenerated
        )))
    }

  implicit def convert(
    out: NetClient.Management.Out.GameJoined
  ): management.GameJoined =
    management.GameJoined(out.human)

  implicit def convert(out: NetClient.Management.Out): management.FromServer =
    out match {
      case msg: NetClient.Management.Out.CheckNameAvailabilityResponse =>
        management.FromServer(checkNameAvailability = Some(msg))
      case msg: NetClient.Management.Out.RegisterResponse =>
        management.FromServer(register = Some(msg))
      case msg: NetClient.Management.Out.LoginResponse =>
        management.FromServer(login = Some(msg))
      case msg: NetClient.Management.Out.GameJoined =>
        management.FromServer(gameJoined = Some(msg))
      case NetClient.Management.Out.JoinGameCancelled =>
        management.FromServer(gameJoinCancelled = Some(management.JoinGameCancelled()))
    }

  implicit def convert(out: NetClient.Msgs.FromServer.TimeSync)
  : messages.TimeSync.FromServer =
    messages.TimeSync.FromServer(clientNow = out.clientNow, serverNow = out.serverNow)

  implicit def convert(out: NetClient.Msgs.FromServer): messages.FromServer =
    out match {
      case msg: NetClient.Msgs.FromServer.Game =>
        messages.FromServer(game = Some(msg.msg))
      case msg: NetClient.Msgs.FromServer.Management =>
        messages.FromServer(management = Some(msg.msg))
      case msg: NetClient.Msgs.FromServer.TimeSync =>
        messages.FromServer(timeSync = Some(msg))
    }

  def serialize(out: NetClient.Msgs.FromServer): ByteString =
    ByteString(convert(out).toByteArray)
}
