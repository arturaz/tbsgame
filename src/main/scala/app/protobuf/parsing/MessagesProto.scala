package app.protobuf.parsing

import java.io.InputStream

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

  def parse(msg: control.Client2Server): String \/ NetClient.Msgs.FromControlClient = {
    import NetClient.Control.In, scala.language.implicitConversions
    implicit def convert(m: control.ControlSecretKey): NetClient.Control.SecretKey =
      NetClient.Control.SecretKey(m.key)

    msg match {
      case control.Client2Server(key, Some(m), _) =>
        NetClient.Msgs.FromControlClient(key, In.Shutdown).right
      case control.Client2Server(key, _, Some(m)) =>
        NetClient.Msgs.FromControlClient(key, In.Status).right
      case control.Client2Server(_, None, None) =>
        s"Empty message: control.Client2Server".left
    }
  }

  def parse(msg: control.Server2Client): String \/ NetClient.Control.Out = {
    import NetClient.Control.Out

    msg match {
      case control.Server2Client(Some(m), _) =>
        Out.GenericReply(m.success, m.message).right
      case control.Server2Client(_, Some(m)) =>
        Out.Status().right
      case control.Server2Client(None, None) =>
        s"Empty message: control.Server2Client".left
    }
  }

  def parse(data: ByteString)
  : String \/ (NetClient.Msgs.FromClient \/ NetClient.Msgs.FromControlClient) = {
    lazy val asClient = parseFromClient(data)
    lazy val asControl = parseFromControlClient(data)

    (asClient.map(_.left) orElse asControl.map(_.right)).leftMap { _ =>
      s"Can't parse data: [asClient=$asClient] [asControl=$asControl]"
    }
  }

  def parseFromClient(data: ByteString): String \/ NetClient.Msgs.FromClient = {
    parseWithParser(data, _ |> messages.FromClient.parseFrom |> parse)
  }

  def parseFromControlClient(data: ByteString): String \/ NetClient.Msgs.FromControlClient = {
    parseWithParser(data, _ |> control.Client2Server.parseFrom |> parse)
  }

  def parseFromControlServer(data: ByteString): String \/ NetClient.Control.Out = {
    parseWithParser(data, _ |> control.Server2Client.parseFrom |> parse)
  }
  
  def parseWithParser[A](data: ByteString, protoParser: InputStream => String \/ A) = {
    try protoParser(data.iterator.asInputStream)
    catch { case e: Exception => s"Error while parsing protobuf: $e".left }
  }
}
