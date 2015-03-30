// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.game

import scala.collection.JavaConversions._
import com.trueaccord.scalapb.Descriptors

final case class TurnEndedEvt(
    teamId: netmsg.game.TeamID
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[TurnEndedEvt] with com.trueaccord.lenses.Updatable[TurnEndedEvt] {
    lazy val serializedSize: Int = {
      var __size = 0
      __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(teamId.serializedSize) + teamId.serializedSize
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      output.writeTag(1, 2)
      output.writeRawVarint32(teamId.serializedSize)
      teamId.writeTo(output)
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.game.TurnEndedEvt = {
      var __teamId = this.teamId
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __teamId = com.trueaccord.scalapb.LiteParser.readMessage(__input, __teamId)
          case tag => __input.skipField(tag)
        }
      }
      netmsg.game.TurnEndedEvt(
          teamId = __teamId
      )
    }
    def withTeamId(__v: netmsg.game.TeamID): TurnEndedEvt = copy(teamId = __v)
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => teamId
      }
    }
    override def toString: String = com.google.protobuf.TextFormat.printToString(netmsg.game.TurnEndedEvt.toJavaProto(this))
    def companion = netmsg.game.TurnEndedEvt
}

object TurnEndedEvt extends com.trueaccord.scalapb.GeneratedMessageCompanion[TurnEndedEvt] with com.trueaccord.scalapb.JavaProtoSupport[TurnEndedEvt, netmsg.Game.TurnEndedEvt]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[TurnEndedEvt] with com.trueaccord.scalapb.JavaProtoSupport[TurnEndedEvt, netmsg.Game.TurnEndedEvt]  = this
  def toJavaProto(scalaPbSource: netmsg.game.TurnEndedEvt): netmsg.Game.TurnEndedEvt = {
    val javaPbOut = netmsg.Game.TurnEndedEvt.newBuilder
    javaPbOut.setTeamId(netmsg.game.TeamID.toJavaProto(scalaPbSource.teamId))
    javaPbOut.build
  }
  def fromJavaProto(javaPbSource: netmsg.Game.TurnEndedEvt): netmsg.game.TurnEndedEvt = netmsg.game.TurnEndedEvt(
    teamId = netmsg.game.TeamID.fromJavaProto(javaPbSource.getTeamId)
  )
  override def fromAscii(ascii: String): netmsg.game.TurnEndedEvt = {
    val javaProtoBuilder = netmsg.Game.TurnEndedEvt.newBuilder
    com.google.protobuf.TextFormat.merge(ascii, javaProtoBuilder)
    fromJavaProto(javaProtoBuilder.build)
  }
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.game.TurnEndedEvt = netmsg.game.TurnEndedEvt(
    teamId = fieldsMap(1).asInstanceOf[netmsg.game.TeamID]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("TurnEndedEvt", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.game.InternalFields_game.internalFieldsFor("netmsg.game.TurnEndedEvt"))
  lazy val defaultInstance = netmsg.game.TurnEndedEvt(
    teamId = netmsg.game.TeamID.defaultInstance
  )
  implicit class TurnEndedEvtLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, TurnEndedEvt]) extends com.trueaccord.lenses.ObjectLens[UpperPB, TurnEndedEvt](_l) {
    def teamId: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.TeamID] = field(_.teamId)((c_, f_) => c_.copy(teamId = f_))
  }
  final val TEAM_ID_FIELD_NUMBER = 1
}
