package app.protobuf.parsing

import java.io.InputStream

import akka.util.ByteString
import app.actors.MsgHandler.Client2Server.BackgroundSFO
import app.actors.NetClient.Msgs.BackgroundSFO
import app.actors.game.GamesManagerActor.BackgroundToken
import app.actors.{MsgHandler, NetClient}
import netmsg._
import implicits._
import spire.math.UInt

import scalaz._, Scalaz._

trait MessagesProto extends BaseProto { _: GameProto with ManagementProto =>
  def parse(m: messages.TimeSync.FromClient): NetClient.Msgs.FromClient.TimeSync =
    NetClient.Msgs.FromClient.TimeSync(parse(m.now))

  def parse(msg: messages.FromClient): String \/ NetClient.Msgs.FromClient = {
    import messages.FromClient

    msg match {
      case FromClient(Some(_), _, _, _) =>
        NetClient.Msgs.FromClient.ProtoVersionCheck.right
      case FromClient(_, Some(m), _, _) =>
        parse(m).map(NetClient.Msgs.FromClient.Game)
      case FromClient(_, _, Some(m), _) =>
        parse(m).map(NetClient.Msgs.FromClient.Management)
      case FromClient(_, _, _, Some(m)) =>
        parse(m).right
      case FromClient(None, None, None, None) =>
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
        Out.Status(
          m.clients.map(UInt.apply), m.playingUsers.map(UInt.apply), m.games.map(UInt.apply)
        ).right
      case control.Server2Client(None, None) =>
        s"Empty message: control.Server2Client".left
    }
  }

  val GameDiscriminator = 0.toByte
  val ControlDiscriminator = 1.toByte
  val BackgroundSFOHeartbeatDiscriminator = 2.toByte
  val BackgroundSFOCancelDiscriminator = 3.toByte

  def parse(data: ByteString): String \/ NetClient.MsgHandlerConnectionIn = {
    data.headOption match {
      case Some(GameDiscriminator) =>
        parseFromClient(data.tail)
      case Some(ControlDiscriminator) =>
        parseFromControlClient(data.tail)
      case Some(
        byte @ (BackgroundSFOHeartbeatDiscriminator | BackgroundSFOCancelDiscriminator)
      ) =>
        val kind =
          if (byte === BackgroundSFOHeartbeatDiscriminator) BackgroundSFO.Kind.Heartbeat
          else BackgroundSFO.Kind.Cancel
        BackgroundSFO(kind, BackgroundToken(data.tail.utf8String)).right
      case Some(other) => s"Unknown discriminator byte: '$other'!".left
      case None => s"Empty data ByteString!".left
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
