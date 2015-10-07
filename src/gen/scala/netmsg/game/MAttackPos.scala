// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.game


import com.trueaccord.scalapb.Descriptors

final case class MAttackPos(
    id: netmsg.game.WObjID,
    targetPos: netmsg.base.Vect2
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[MAttackPos] with com.trueaccord.lenses.Updatable[MAttackPos] {
    lazy val serializedSize: Int = {
      var __size = 0
      __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(id.serializedSize) + id.serializedSize
      __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(targetPos.serializedSize) + targetPos.serializedSize
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      output.writeTag(1, 2)
      output.writeRawVarint32(id.serializedSize)
      id.writeTo(output)
      output.writeTag(2, 2)
      output.writeRawVarint32(targetPos.serializedSize)
      targetPos.writeTo(output)
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.game.MAttackPos = {
      var __id = this.id
      var __targetPos = this.targetPos
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __id = com.trueaccord.scalapb.LiteParser.readMessage(__input, __id)
          case 18 =>
            __targetPos = com.trueaccord.scalapb.LiteParser.readMessage(__input, __targetPos)
          case tag => __input.skipField(tag)
        }
      }
      netmsg.game.MAttackPos(
          id = __id,
          targetPos = __targetPos
      )
    }
    def withId(__v: netmsg.game.WObjID): MAttackPos = copy(id = __v)
    def withTargetPos(__v: netmsg.base.Vect2): MAttackPos = copy(targetPos = __v)
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => id
        case 2 => targetPos
      }
    }
    def companion = netmsg.game.MAttackPos
}

object MAttackPos extends com.trueaccord.scalapb.GeneratedMessageCompanion[MAttackPos]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[MAttackPos]  = this
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.game.MAttackPos = netmsg.game.MAttackPos(
    id = fieldsMap(1).asInstanceOf[netmsg.game.WObjID],
    targetPos = fieldsMap(2).asInstanceOf[netmsg.base.Vect2]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("MAttackPos", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.game.InternalFields_game.internalFieldsFor("netmsg.game.MAttackPos"))
  lazy val defaultInstance = netmsg.game.MAttackPos(
    id = netmsg.game.WObjID.defaultInstance,
    targetPos = netmsg.base.Vect2.defaultInstance
  )
  implicit class MAttackPosLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, MAttackPos]) extends com.trueaccord.lenses.ObjectLens[UpperPB, MAttackPos](_l) {
    def id: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObjID] = field(_.id)((c_, f_) => c_.copy(id = f_))
    def targetPos: com.trueaccord.lenses.Lens[UpperPB, netmsg.base.Vect2] = field(_.targetPos)((c_, f_) => c_.copy(targetPos = f_))
  }
  final val ID_FIELD_NUMBER = 1
  final val TARGET_POS_FIELD_NUMBER = 2
}
