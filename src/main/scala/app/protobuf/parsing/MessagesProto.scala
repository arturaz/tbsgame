package app.protobuf.parsing

import java.io.InputStream

import akka.util.ByteString
import app.actors.NetClient.NotLoggedInState.BackgroundLogin
import app.actors.game.GamesManagerActor.BackgroundToken
import app.actors.net_client.ControlSecretKey
import app.actors.NetClient
import netmsg._
import implicits._
import spire.math.UInt

import scalaz._, Scalaz._

trait MessagesProto extends BaseProto { _: GameProto with ManagementProto =>
  def parse(m: messages.TimeSync.FromClient): NetClient.MsgHandlerConnectionIn.TimeSync =
    NetClient.MsgHandlerConnectionIn.TimeSync(parse(m.now))

  def parse(msg: messages.FromClient): String \/ NetClient.GameClientIn = {
    import messages.FromClient

    msg match {
      case FromClient(Some(_), _, _, _) =>
        NetClient.NotLoggedInState.ProtoVersionCheck.right
      case FromClient(_, Some(m), _, _) =>
        parse(m).map(NetClient.InGameState.FromMsgHandler)
      case FromClient(_, _, Some(m), _) =>
        parse(m)
      case FromClient(_, _, _, Some(m)) =>
        parse(m).right
      case FromClient(None, None, None, None) =>
        s"Empty message messages.FromClient!".left
    }
  }

  def parse(msg: control.Client2Server): String \/ NetClient.Control = {
    import NetClient.Control, scala.language.implicitConversions
    implicit def convert(m: control.ControlSecretKey): ControlSecretKey =
      ControlSecretKey(m.key)

    msg match {
      case control.Client2Server(key, Some(m), _) =>
        Control(key, Control.In.Shutdown).right
      case control.Client2Server(key, _, Some(m)) =>
        Control(key, Control.In.Status).right
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

  def parse(msg: bg_client.Client2Server): String \/ NetClient.BackgroundClientIn = {
    msg match {
      case bg_client.Client2Server(Some(m)) =>
        BackgroundLogin(BackgroundToken(m.token)).right
      case bg_client.Client2Server(None) =>
        s"Empty message: bg_client.Client2Server".left
    }
  }

  val GameDiscriminator = 0.toByte
  val ControlDiscriminator = 1.toByte
  val BackgroundClientDiscriminator = 2.toByte

  def parse(data: ByteString): String \/ NetClient.MsgHandlerConnectionIn = {
    data.headOption match {
      case Some(GameDiscriminator) =>
        parseFromClient(data.tail)
      case Some(ControlDiscriminator) =>
        parseFromControlClient(data.tail)
      case Some(BackgroundClientDiscriminator) =>
        parseFromBackgroundClient(data.tail)
      case Some(other) => s"Unknown discriminator byte: '$other'!".left
      case None => s"Empty data ByteString!".left
    }
  }

  def parseFromClient(data: ByteString): String \/ NetClient.GameClientIn = {
    parseWithParser(data, _ |> messages.FromClient.parseFrom |> parse)
  }

  def parseFromControlClient(data: ByteString): String \/ NetClient.Control = {
    parseWithParser(data, _ |> control.Client2Server.parseFrom |> parse)
  }

  def parseFromBackgroundClient(data: ByteString): String \/ NetClient.BackgroundClientIn = {
    parseWithParser(data, _ |> bg_client.Client2Server.parseFrom |> parse)
  }

  def parseFromControlServer(data: ByteString): String \/ NetClient.Control.Out = {
    parseWithParser(data, _ |> control.Server2Client.parseFrom |> parse)
  }
  
  def parseWithParser[A](data: ByteString, protoParser: InputStream => String \/ A) = {
    try protoParser(data.iterator.asInputStream)
    catch { case e: Exception => s"Error while parsing protobuf data $data: $e".left }
  }
}
