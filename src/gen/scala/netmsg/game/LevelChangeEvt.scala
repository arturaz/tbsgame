// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.game

import scala.collection.JavaConversions._
import com.trueaccord.scalapb.Descriptors

final case class LevelChangeEvt(
    objId: netmsg.game.WObjID,
    newLevel: Int
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[LevelChangeEvt] with com.trueaccord.lenses.Updatable[LevelChangeEvt] {
    lazy val serializedSize: Int = {
      var __size = 0
      __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(objId.serializedSize) + objId.serializedSize
      __size += com.google.protobuf.CodedOutputStream.computeUInt32Size(2, newLevel)
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      output.writeTag(1, 2)
      output.writeRawVarint32(objId.serializedSize)
      objId.writeTo(output)
      output.writeUInt32(2, newLevel)
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.game.LevelChangeEvt = {
      var __objId = this.objId
      var __newLevel = this.newLevel
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __objId = com.trueaccord.scalapb.LiteParser.readMessage(__input, __objId)
          case 16 =>
            __newLevel = __input.readUInt32()
          case tag => __input.skipField(tag)
        }
      }
      netmsg.game.LevelChangeEvt(
          objId = __objId,
          newLevel = __newLevel
      )
    }
    def withObjId(__v: netmsg.game.WObjID): LevelChangeEvt = copy(objId = __v)
    def withNewLevel(__v: Int): LevelChangeEvt = copy(newLevel = __v)
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => objId
        case 2 => newLevel
      }
    }
    override def toString: String = com.google.protobuf.TextFormat.printToString(netmsg.game.LevelChangeEvt.toJavaProto(this))
    def companion = netmsg.game.LevelChangeEvt
}

object LevelChangeEvt extends com.trueaccord.scalapb.GeneratedMessageCompanion[LevelChangeEvt] with com.trueaccord.scalapb.JavaProtoSupport[LevelChangeEvt, netmsg.Game.LevelChangeEvt]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[LevelChangeEvt] with com.trueaccord.scalapb.JavaProtoSupport[LevelChangeEvt, netmsg.Game.LevelChangeEvt]  = this
  def toJavaProto(scalaPbSource: netmsg.game.LevelChangeEvt): netmsg.Game.LevelChangeEvt = {
    val javaPbOut = netmsg.Game.LevelChangeEvt.newBuilder
    javaPbOut.setObjId(netmsg.game.WObjID.toJavaProto(scalaPbSource.objId))
    javaPbOut.setNewLevel(scalaPbSource.newLevel)
    javaPbOut.build
  }
  def fromJavaProto(javaPbSource: netmsg.Game.LevelChangeEvt): netmsg.game.LevelChangeEvt = netmsg.game.LevelChangeEvt(
    objId = netmsg.game.WObjID.fromJavaProto(javaPbSource.getObjId),
    newLevel = javaPbSource.getNewLevel.intValue
  )
  override def fromAscii(ascii: String): netmsg.game.LevelChangeEvt = {
    val javaProtoBuilder = netmsg.Game.LevelChangeEvt.newBuilder
    com.google.protobuf.TextFormat.merge(ascii, javaProtoBuilder)
    fromJavaProto(javaProtoBuilder.build)
  }
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.game.LevelChangeEvt = netmsg.game.LevelChangeEvt(
    objId = fieldsMap(1).asInstanceOf[netmsg.game.WObjID],
    newLevel = fieldsMap(2).asInstanceOf[Int]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("LevelChangeEvt", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.game.InternalFields_game.internalFieldsFor("netmsg.game.LevelChangeEvt"))
  lazy val defaultInstance = netmsg.game.LevelChangeEvt(
    objId = netmsg.game.WObjID.defaultInstance,
    newLevel = 0
  )
  implicit class LevelChangeEvtLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, LevelChangeEvt]) extends com.trueaccord.lenses.ObjectLens[UpperPB, LevelChangeEvt](_l) {
    def objId: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObjID] = field(_.objId)((c_, f_) => c_.copy(objId = f_))
    def newLevel: com.trueaccord.lenses.Lens[UpperPB, Int] = field(_.newLevel)((c_, f_) => c_.copy(newLevel = f_))
  }
  final val OBJ_ID_FIELD_NUMBER = 1
  final val NEW_LEVEL_FIELD_NUMBER = 2
}
