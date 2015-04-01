// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.game


import com.trueaccord.scalapb.Descriptors

final case class MWarp(
    position: netmsg.base.Vect2,
    warpable: netmsg.game.WarpableKind
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[MWarp] with com.trueaccord.lenses.Updatable[MWarp] {
    lazy val serializedSize: Int = {
      var __size = 0
      __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(position.serializedSize) + position.serializedSize
      __size += com.google.protobuf.CodedOutputStream.computeEnumSize(2, warpable.id)
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      output.writeTag(1, 2)
      output.writeRawVarint32(position.serializedSize)
      position.writeTo(output)
      output.writeEnum(2, warpable.id)
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.game.MWarp = {
      var __position = this.position
      var __warpable = this.warpable
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __position = com.trueaccord.scalapb.LiteParser.readMessage(__input, __position)
          case 16 =>
            __warpable = netmsg.game.WarpableKind.fromValue(__input.readEnum())
          case tag => __input.skipField(tag)
        }
      }
      netmsg.game.MWarp(
          position = __position,
          warpable = __warpable
      )
    }
    def withPosition(__v: netmsg.base.Vect2): MWarp = copy(position = __v)
    def withWarpable(__v: netmsg.game.WarpableKind): MWarp = copy(warpable = __v)
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => position
        case 2 => warpable
      }
    }
    def companion = netmsg.game.MWarp
}

object MWarp extends com.trueaccord.scalapb.GeneratedMessageCompanion[MWarp]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[MWarp]  = this
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.game.MWarp = netmsg.game.MWarp(
    position = fieldsMap(1).asInstanceOf[netmsg.base.Vect2],
    warpable = fieldsMap(2).asInstanceOf[netmsg.game.WarpableKind]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("MWarp", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.game.InternalFields_game.internalFieldsFor("netmsg.game.MWarp"))
  lazy val defaultInstance = netmsg.game.MWarp(
    position = netmsg.base.Vect2.defaultInstance,
    warpable = netmsg.game.WarpableKind.B_EXTRACTOR
  )
  implicit class MWarpLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, MWarp]) extends com.trueaccord.lenses.ObjectLens[UpperPB, MWarp](_l) {
    def position: com.trueaccord.lenses.Lens[UpperPB, netmsg.base.Vect2] = field(_.position)((c_, f_) => c_.copy(position = f_))
    def warpable: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WarpableKind] = field(_.warpable)((c_, f_) => c_.copy(warpable = f_))
  }
  final val POSITION_FIELD_NUMBER = 1
  final val WARPABLE_FIELD_NUMBER = 2
}
