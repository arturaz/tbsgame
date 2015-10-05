package app.protobuf.parsing

import akka.util.ByteString
import app.actors.NetClient
import netmsg._
import implicits._

import scalaz._, Scalaz._

trait MessagesProto extends BaseProto { _: GameProto with ManagementProto =>
  def parse(m: messages.TimeSync.FromClient): NetClient.Msgs.FromClient.TimeSync =
    NetClient.Msgs.FromClient.TimeSync(parse(m.now))

  def parse(msg: messages.FromClient): String \/ NetClient.Msgs.FromClient = {
    import messages.FromClient

    msg match {
      case FromClient(Some(m), _, _) =>
        parse(m).map(NetClient.Msgs.FromClient.Game)
      case FromClient(_, Some(m), _) =>
        parse(m).map(NetClient.Msgs.FromClient.Management)
      case FromClient(_, _, Some(m)) =>
        parse(m).right
      case FromClient(None, None, None) =>
        s"Empty message $msg!".left
    }
  }

  def parse(data: ByteString): String \/ NetClient.Msgs.FromClient = {
    try {
      val protoMsg = messages.FromClient.parseFrom(data.iterator.asInputStream)
      parse(protoMsg)
    }
    catch {
      case e: Throwable => s"Error while parsing protobuf: $e".left
    }
  }
}
