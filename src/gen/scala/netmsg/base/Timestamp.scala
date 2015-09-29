// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.base


import com.trueaccord.scalapb.Descriptors

final case class Timestamp(
    timestamp: Long
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[Timestamp] with com.trueaccord.lenses.Updatable[Timestamp] {
    lazy val serializedSize: Int = {
      var __size = 0
      __size += com.google.protobuf.CodedOutputStream.computeUInt64Size(1, timestamp)
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      output.writeUInt64(1, timestamp)
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.base.Timestamp = {
      var __timestamp = this.timestamp
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 8 =>
            __timestamp = __input.readUInt64()
          case tag => __input.skipField(tag)
        }
      }
      netmsg.base.Timestamp(
          timestamp = __timestamp
      )
    }
    def withTimestamp(__v: Long): Timestamp = copy(timestamp = __v)
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => timestamp
      }
    }
    def companion = netmsg.base.Timestamp
}

object Timestamp extends com.trueaccord.scalapb.GeneratedMessageCompanion[Timestamp]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[Timestamp]  = this
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.base.Timestamp = netmsg.base.Timestamp(
    timestamp = fieldsMap(1).asInstanceOf[Long]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("Timestamp", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.base.InternalFields_base.internalFieldsFor("netmsg.base.Timestamp"))
  lazy val defaultInstance = netmsg.base.Timestamp(
    timestamp = 0L
  )
  implicit class TimestampLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, Timestamp]) extends com.trueaccord.lenses.ObjectLens[UpperPB, Timestamp](_l) {
    def timestamp: com.trueaccord.lenses.Lens[UpperPB, Long] = field(_.timestamp)((c_, f_) => c_.copy(timestamp = f_))
  }
  final val TIMESTAMP_FIELD_NUMBER = 1
}