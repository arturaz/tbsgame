// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.control


import com.trueaccord.scalapb.Descriptors

final case class ShutdownReq(
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[ShutdownReq] with com.trueaccord.lenses.Updatable[ShutdownReq] {
    lazy val serializedSize: Int = {
      var __size = 0
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.control.ShutdownReq = {
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case tag => __input.skipField(tag)
        }
      }
      netmsg.control.ShutdownReq(
      )
    }
    def getField(__field: Descriptors.FieldDescriptor): Any = throw new MatchError(__field)
    def companion = netmsg.control.ShutdownReq
}

object ShutdownReq extends com.trueaccord.scalapb.GeneratedMessageCompanion[ShutdownReq]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[ShutdownReq]  = this
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.control.ShutdownReq = netmsg.control.ShutdownReq(
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("ShutdownReq", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.control.InternalFields_control.internalFieldsFor("netmsg.control.ShutdownReq"))
  lazy val defaultInstance = netmsg.control.ShutdownReq(
  )
  implicit class ShutdownReqLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, ShutdownReq]) extends com.trueaccord.lenses.ObjectLens[UpperPB, ShutdownReq](_l) {
  }
}