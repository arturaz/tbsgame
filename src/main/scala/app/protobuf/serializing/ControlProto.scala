package app.protobuf.serializing

import akka.util.ByteString
import app.actors.NetClient
import app.actors.net_client.ControlSecretKey
import netmsg.control
import scala.language.implicitConversions

trait ControlProto { _: Serializing =>
  implicit def convert(msg: NetClient.Control): control.Client2Server = {
    implicit def convert(key: ControlSecretKey): control.ControlSecretKey =
      control.ControlSecretKey(key.key)

    msg.msg match {
      case NetClient.Control.In.Shutdown =>
        control.Client2Server(msg.key, shutdown = Some(control.ShutdownReq()))
      case NetClient.Control.In.Status =>
        control.Client2Server(msg.key, status = Some(control.StatusReq()))
    }
  }

  implicit def convert(out: NetClient.Control.Out): control.Server2Client =
    out match {
      case NetClient.Control.Out.GenericReply(success, messageOpt) =>
        control.Server2Client(reply = Some(control.GenericReply(success, messageOpt)))
      case NetClient.Control.Out.Status(clients, playingUsers, games) =>
        control.Server2Client(status = Some(control.StatusReply(
          clients.map(convert), playingUsers.map(convert), games.map(convert)
        )))
    }

  def serializeControl(out: NetClient.Control): ByteString = serializeGenMsg(convert(out))
}
