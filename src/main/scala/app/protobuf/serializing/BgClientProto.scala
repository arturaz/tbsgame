package app.protobuf.serializing

import app.actors.NetClient
import netmsg.bg_client

import scala.language.implicitConversions

trait BgClientProto { _: Serializing =>
  def convert(msg: NetClient.BackgroundClientOut): bg_client.Server2Client = msg match {
    case NetClient.NotLoggedInState.BackgroundLoginReply(errorOrToken) =>
      bg_client.Server2Client(login = Some(bg_client.LoginReply(errorOrToken.swap.toOption)))
    case NetClient.BackgroundLoggedInState.WaitingListChanged(opponentWaiting) =>
      bg_client.Server2Client(waitingListChanged = Some(bg_client.WaitingListChanged(opponentWaiting)))
  }
}
