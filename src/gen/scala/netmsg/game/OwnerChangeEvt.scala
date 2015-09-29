// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.game


import com.trueaccord.scalapb.Descriptors

final case class OwnerChangeEvt(
    objId: netmsg.game.WObjID,
    newOwnerId: netmsg.game.OwnerID
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[OwnerChangeEvt] with com.trueaccord.lenses.Updatable[OwnerChangeEvt] {
    lazy val serializedSize: Int = {
      var __size = 0
      __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(objId.serializedSize) + objId.serializedSize
      __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(newOwnerId.serializedSize) + newOwnerId.serializedSize
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      output.writeTag(1, 2)
      output.writeRawVarint32(objId.serializedSize)
      objId.writeTo(output)
      output.writeTag(2, 2)
      output.writeRawVarint32(newOwnerId.serializedSize)
      newOwnerId.writeTo(output)
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.game.OwnerChangeEvt = {
      var __objId = this.objId
      var __newOwnerId = this.newOwnerId
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __objId = com.trueaccord.scalapb.LiteParser.readMessage(__input, __objId)
          case 18 =>
            __newOwnerId = com.trueaccord.scalapb.LiteParser.readMessage(__input, __newOwnerId)
          case tag => __input.skipField(tag)
        }
      }
      netmsg.game.OwnerChangeEvt(
          objId = __objId,
          newOwnerId = __newOwnerId
      )
    }
    def withObjId(__v: netmsg.game.WObjID): OwnerChangeEvt = copy(objId = __v)
    def withNewOwnerId(__v: netmsg.game.OwnerID): OwnerChangeEvt = copy(newOwnerId = __v)
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => objId
        case 2 => newOwnerId
      }
    }
    def companion = netmsg.game.OwnerChangeEvt
}

object OwnerChangeEvt extends com.trueaccord.scalapb.GeneratedMessageCompanion[OwnerChangeEvt]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[OwnerChangeEvt]  = this
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.game.OwnerChangeEvt = netmsg.game.OwnerChangeEvt(
    objId = fieldsMap(1).asInstanceOf[netmsg.game.WObjID],
    newOwnerId = fieldsMap(2).asInstanceOf[netmsg.game.OwnerID]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("OwnerChangeEvt", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.game.InternalFields_game.internalFieldsFor("netmsg.game.OwnerChangeEvt"))
  lazy val defaultInstance = netmsg.game.OwnerChangeEvt(
    objId = netmsg.game.WObjID.defaultInstance,
    newOwnerId = netmsg.game.OwnerID.defaultInstance
  )
  implicit class OwnerChangeEvtLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, OwnerChangeEvt]) extends com.trueaccord.lenses.ObjectLens[UpperPB, OwnerChangeEvt](_l) {
    def objId: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObjID] = field(_.objId)((c_, f_) => c_.copy(objId = f_))
    def newOwnerId: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.OwnerID] = field(_.newOwnerId)((c_, f_) => c_.copy(newOwnerId = f_))
  }
  final val OBJ_ID_FIELD_NUMBER = 1
  final val NEW_OWNER_ID_FIELD_NUMBER = 2
}