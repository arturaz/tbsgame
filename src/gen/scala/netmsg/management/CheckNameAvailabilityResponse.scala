// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.management

import scala.collection.JavaConversions._
import com.trueaccord.scalapb.Descriptors

final case class CheckNameAvailabilityResponse(
    name: String,
    available: Boolean
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[CheckNameAvailabilityResponse] with com.trueaccord.lenses.Updatable[CheckNameAvailabilityResponse] {
    lazy val serializedSize: Int = {
      var __size = 0
      __size += com.google.protobuf.CodedOutputStream.computeStringSize(1, name)
      __size += com.google.protobuf.CodedOutputStream.computeBoolSize(2, available)
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      output.writeString(1, name)
      output.writeBool(2, available)
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.management.CheckNameAvailabilityResponse = {
      var __name = this.name
      var __available = this.available
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __name = __input.readString()
          case 16 =>
            __available = __input.readBool()
          case tag => __input.skipField(tag)
        }
      }
      netmsg.management.CheckNameAvailabilityResponse(
          name = __name,
          available = __available
      )
    }
    def withName(__v: String): CheckNameAvailabilityResponse = copy(name = __v)
    def withAvailable(__v: Boolean): CheckNameAvailabilityResponse = copy(available = __v)
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => name
        case 2 => available
      }
    }
    override def toString: String = com.google.protobuf.TextFormat.printToString(netmsg.management.CheckNameAvailabilityResponse.toJavaProto(this))
    def companion = netmsg.management.CheckNameAvailabilityResponse
}

object CheckNameAvailabilityResponse extends com.trueaccord.scalapb.GeneratedMessageCompanion[CheckNameAvailabilityResponse] with com.trueaccord.scalapb.JavaProtoSupport[CheckNameAvailabilityResponse, netmsg.Management.CheckNameAvailabilityResponse]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[CheckNameAvailabilityResponse] with com.trueaccord.scalapb.JavaProtoSupport[CheckNameAvailabilityResponse, netmsg.Management.CheckNameAvailabilityResponse]  = this
  def toJavaProto(scalaPbSource: netmsg.management.CheckNameAvailabilityResponse): netmsg.Management.CheckNameAvailabilityResponse = {
    val javaPbOut = netmsg.Management.CheckNameAvailabilityResponse.newBuilder
    javaPbOut.setName(scalaPbSource.name)
    javaPbOut.setAvailable(scalaPbSource.available)
    javaPbOut.build
  }
  def fromJavaProto(javaPbSource: netmsg.Management.CheckNameAvailabilityResponse): netmsg.management.CheckNameAvailabilityResponse = netmsg.management.CheckNameAvailabilityResponse(
    name = javaPbSource.getName,
    available = javaPbSource.getAvailable.booleanValue
  )
  override def fromAscii(ascii: String): netmsg.management.CheckNameAvailabilityResponse = {
    val javaProtoBuilder = netmsg.Management.CheckNameAvailabilityResponse.newBuilder
    com.google.protobuf.TextFormat.merge(ascii, javaProtoBuilder)
    fromJavaProto(javaProtoBuilder.build)
  }
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.management.CheckNameAvailabilityResponse = netmsg.management.CheckNameAvailabilityResponse(
    name = fieldsMap(1).asInstanceOf[String],
    available = fieldsMap(2).asInstanceOf[Boolean]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("CheckNameAvailabilityResponse", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.management.InternalFields_management.internalFieldsFor("netmsg.management.CheckNameAvailabilityResponse"))
  lazy val defaultInstance = netmsg.management.CheckNameAvailabilityResponse(
    name = "",
    available = false
  )
  implicit class CheckNameAvailabilityResponseLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, CheckNameAvailabilityResponse]) extends com.trueaccord.lenses.ObjectLens[UpperPB, CheckNameAvailabilityResponse](_l) {
    def name: com.trueaccord.lenses.Lens[UpperPB, String] = field(_.name)((c_, f_) => c_.copy(name = f_))
    def available: com.trueaccord.lenses.Lens[UpperPB, Boolean] = field(_.available)((c_, f_) => c_.copy(available = f_))
  }
  final val NAME_FIELD_NUMBER = 1
  final val AVAILABLE_FIELD_NUMBER = 2
}
