// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.game


import com.trueaccord.scalapb.Descriptors

final case class MLeave(
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[MLeave] with com.trueaccord.lenses.Updatable[MLeave] {
    lazy val serializedSize: Int = {
      var __size = 0
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.game.MLeave = {
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case tag => __input.skipField(tag)
        }
      }
      netmsg.game.MLeave(
      )
    }
    def getField(__field: Descriptors.FieldDescriptor): Any = throw new MatchError(__field)
    def companion = netmsg.game.MLeave
}

object MLeave extends com.trueaccord.scalapb.GeneratedMessageCompanion[MLeave]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[MLeave]  = this
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.game.MLeave = netmsg.game.MLeave(
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("MLeave", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.game.InternalFields_game.internalFieldsFor("netmsg.game.MLeave"))
  lazy val defaultInstance = netmsg.game.MLeave(
  )
  implicit class MLeaveLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, MLeave]) extends com.trueaccord.lenses.ObjectLens[UpperPB, MLeave](_l) {
  }
}
