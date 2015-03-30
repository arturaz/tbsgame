// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.messages

import scala.collection.JavaConversions._
import com.trueaccord.scalapb.Descriptors

final case class FromClient(
    game: Option[netmsg.game.FromClient] = None,
    management: Option[netmsg.management.FromClient] = None,
    timeSync: Option[netmsg.messages.TimeSync.FromClient] = None
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[FromClient] with com.trueaccord.lenses.Updatable[FromClient] {
    lazy val serializedSize: Int = {
      var __size = 0
      if (game.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(game.get.serializedSize) + game.get.serializedSize }
      if (management.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(management.get.serializedSize) + management.get.serializedSize }
      if (timeSync.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(timeSync.get.serializedSize) + timeSync.get.serializedSize }
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      game.foreach { v => 
        output.writeTag(1, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      management.foreach { v => 
        output.writeTag(2, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      timeSync.foreach { v => 
        output.writeTag(3, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.messages.FromClient = {
      var __game = this.game
      var __management = this.management
      var __timeSync = this.timeSync
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __game = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __game.getOrElse(netmsg.game.FromClient.defaultInstance)))
          case 18 =>
            __management = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __management.getOrElse(netmsg.management.FromClient.defaultInstance)))
          case 26 =>
            __timeSync = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __timeSync.getOrElse(netmsg.messages.TimeSync.FromClient.defaultInstance)))
          case tag => __input.skipField(tag)
        }
      }
      netmsg.messages.FromClient(
          game = __game,
          management = __management,
          timeSync = __timeSync
      )
    }
    def getGame: netmsg.game.FromClient = game.getOrElse(netmsg.game.FromClient.defaultInstance)
    def clearGame: FromClient = copy(game = None)
    def withGame(__v: netmsg.game.FromClient): FromClient = copy(game = Some(__v))
    def getManagement: netmsg.management.FromClient = management.getOrElse(netmsg.management.FromClient.defaultInstance)
    def clearManagement: FromClient = copy(management = None)
    def withManagement(__v: netmsg.management.FromClient): FromClient = copy(management = Some(__v))
    def getTimeSync: netmsg.messages.TimeSync.FromClient = timeSync.getOrElse(netmsg.messages.TimeSync.FromClient.defaultInstance)
    def clearTimeSync: FromClient = copy(timeSync = None)
    def withTimeSync(__v: netmsg.messages.TimeSync.FromClient): FromClient = copy(timeSync = Some(__v))
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => game
        case 2 => management
        case 3 => timeSync
      }
    }
    override def toString: String = com.google.protobuf.TextFormat.printToString(netmsg.messages.FromClient.toJavaProto(this))
    def companion = netmsg.messages.FromClient
}

object FromClient extends com.trueaccord.scalapb.GeneratedMessageCompanion[FromClient] with com.trueaccord.scalapb.JavaProtoSupport[FromClient, netmsg.Messages.FromClient]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[FromClient] with com.trueaccord.scalapb.JavaProtoSupport[FromClient, netmsg.Messages.FromClient]  = this
  def toJavaProto(scalaPbSource: netmsg.messages.FromClient): netmsg.Messages.FromClient = {
    val javaPbOut = netmsg.Messages.FromClient.newBuilder
    scalaPbSource.game.map(netmsg.game.FromClient.toJavaProto).foreach(javaPbOut.setGame)
    scalaPbSource.management.map(netmsg.management.FromClient.toJavaProto).foreach(javaPbOut.setManagement)
    scalaPbSource.timeSync.map(netmsg.messages.TimeSync.FromClient.toJavaProto).foreach(javaPbOut.setTimeSync)
    javaPbOut.build
  }
  def fromJavaProto(javaPbSource: netmsg.Messages.FromClient): netmsg.messages.FromClient = netmsg.messages.FromClient(
    game = if (javaPbSource.hasGame) Some(netmsg.game.FromClient.fromJavaProto(javaPbSource.getGame)) else None,
    management = if (javaPbSource.hasManagement) Some(netmsg.management.FromClient.fromJavaProto(javaPbSource.getManagement)) else None,
    timeSync = if (javaPbSource.hasTimeSync) Some(netmsg.messages.TimeSync.FromClient.fromJavaProto(javaPbSource.getTimeSync)) else None
  )
  override def fromAscii(ascii: String): netmsg.messages.FromClient = {
    val javaProtoBuilder = netmsg.Messages.FromClient.newBuilder
    com.google.protobuf.TextFormat.merge(ascii, javaProtoBuilder)
    fromJavaProto(javaProtoBuilder.build)
  }
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.messages.FromClient = netmsg.messages.FromClient(
    game = fieldsMap.getOrElse(1, None).asInstanceOf[Option[netmsg.game.FromClient]],
    management = fieldsMap.getOrElse(2, None).asInstanceOf[Option[netmsg.management.FromClient]],
    timeSync = fieldsMap.getOrElse(3, None).asInstanceOf[Option[netmsg.messages.TimeSync.FromClient]]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("FromClient", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.messages.InternalFields_messages.internalFieldsFor("netmsg.messages.FromClient"))
  lazy val defaultInstance = netmsg.messages.FromClient(
  )
  implicit class FromClientLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, FromClient]) extends com.trueaccord.lenses.ObjectLens[UpperPB, FromClient](_l) {
    def game: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.FromClient] = field(_.getGame)((c_, f_) => c_.copy(game = Some(f_)))
    def optionalGame: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.FromClient]] = field(_.game)((c_, f_) => c_.copy(game = f_))
    def management: com.trueaccord.lenses.Lens[UpperPB, netmsg.management.FromClient] = field(_.getManagement)((c_, f_) => c_.copy(management = Some(f_)))
    def optionalManagement: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.management.FromClient]] = field(_.management)((c_, f_) => c_.copy(management = f_))
    def timeSync: com.trueaccord.lenses.Lens[UpperPB, netmsg.messages.TimeSync.FromClient] = field(_.getTimeSync)((c_, f_) => c_.copy(timeSync = Some(f_)))
    def optionalTimeSync: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.messages.TimeSync.FromClient]] = field(_.timeSync)((c_, f_) => c_.copy(timeSync = f_))
  }
  final val GAME_FIELD_NUMBER = 1
  final val MANAGEMENT_FIELD_NUMBER = 2
  final val TIME_SYNC_FIELD_NUMBER = 3
}
