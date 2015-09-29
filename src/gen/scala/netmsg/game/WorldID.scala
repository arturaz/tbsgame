// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.game


import com.trueaccord.scalapb.Descriptors

final case class WorldID(
    id: netmsg.base.UUID
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[WorldID] with com.trueaccord.lenses.Updatable[WorldID] {
    lazy val serializedSize: Int = {
      var __size = 0
      __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(id.serializedSize) + id.serializedSize
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      output.writeTag(1, 2)
      output.writeRawVarint32(id.serializedSize)
      id.writeTo(output)
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.game.WorldID = {
      var __id = this.id
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __id = com.trueaccord.scalapb.LiteParser.readMessage(__input, __id)
          case tag => __input.skipField(tag)
        }
      }
      netmsg.game.WorldID(
          id = __id
      )
    }
    def withId(__v: netmsg.base.UUID): WorldID = copy(id = __v)
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => id
      }
    }
    def companion = netmsg.game.WorldID
}

object WorldID extends com.trueaccord.scalapb.GeneratedMessageCompanion[WorldID]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[WorldID]  = this
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.game.WorldID = netmsg.game.WorldID(
    id = fieldsMap(1).asInstanceOf[netmsg.base.UUID]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("WorldID", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.game.InternalFields_game.internalFieldsFor("netmsg.game.WorldID"))
  lazy val defaultInstance = netmsg.game.WorldID(
    id = netmsg.base.UUID.defaultInstance
  )
  implicit class WorldIDLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, WorldID]) extends com.trueaccord.lenses.ObjectLens[UpperPB, WorldID](_l) {
    def id: com.trueaccord.lenses.Lens[UpperPB, netmsg.base.UUID] = field(_.id)((c_, f_) => c_.copy(id = f_))
  }
  final val ID_FIELD_NUMBER = 1
}