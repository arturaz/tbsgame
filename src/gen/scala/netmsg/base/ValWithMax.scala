// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.base

import scala.collection.JavaConversions._
import com.trueaccord.scalapb.Descriptors

final case class ValWithMax(
    current: Int,
    maximum: Int
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[ValWithMax] with com.trueaccord.lenses.Updatable[ValWithMax] {
    lazy val serializedSize: Int = {
      var __size = 0
      __size += com.google.protobuf.CodedOutputStream.computeUInt32Size(1, current)
      __size += com.google.protobuf.CodedOutputStream.computeUInt32Size(2, maximum)
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      output.writeUInt32(1, current)
      output.writeUInt32(2, maximum)
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.base.ValWithMax = {
      var __current = this.current
      var __maximum = this.maximum
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 8 =>
            __current = __input.readUInt32()
          case 16 =>
            __maximum = __input.readUInt32()
          case tag => __input.skipField(tag)
        }
      }
      netmsg.base.ValWithMax(
          current = __current,
          maximum = __maximum
      )
    }
    def withCurrent(__v: Int): ValWithMax = copy(current = __v)
    def withMaximum(__v: Int): ValWithMax = copy(maximum = __v)
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => current
        case 2 => maximum
      }
    }
    override def toString: String = com.google.protobuf.TextFormat.printToString(netmsg.base.ValWithMax.toJavaProto(this))
    def companion = netmsg.base.ValWithMax
}

object ValWithMax extends com.trueaccord.scalapb.GeneratedMessageCompanion[ValWithMax] with com.trueaccord.scalapb.JavaProtoSupport[ValWithMax, netmsg.Base.ValWithMax]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[ValWithMax] with com.trueaccord.scalapb.JavaProtoSupport[ValWithMax, netmsg.Base.ValWithMax]  = this
  def toJavaProto(scalaPbSource: netmsg.base.ValWithMax): netmsg.Base.ValWithMax = {
    val javaPbOut = netmsg.Base.ValWithMax.newBuilder
    javaPbOut.setCurrent(scalaPbSource.current)
    javaPbOut.setMaximum(scalaPbSource.maximum)
    javaPbOut.build
  }
  def fromJavaProto(javaPbSource: netmsg.Base.ValWithMax): netmsg.base.ValWithMax = netmsg.base.ValWithMax(
    current = javaPbSource.getCurrent.intValue,
    maximum = javaPbSource.getMaximum.intValue
  )
  override def fromAscii(ascii: String): netmsg.base.ValWithMax = {
    val javaProtoBuilder = netmsg.Base.ValWithMax.newBuilder
    com.google.protobuf.TextFormat.merge(ascii, javaProtoBuilder)
    fromJavaProto(javaProtoBuilder.build)
  }
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.base.ValWithMax = netmsg.base.ValWithMax(
    current = fieldsMap(1).asInstanceOf[Int],
    maximum = fieldsMap(2).asInstanceOf[Int]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("ValWithMax", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.base.InternalFields_base.internalFieldsFor("netmsg.base.ValWithMax"))
  lazy val defaultInstance = netmsg.base.ValWithMax(
    current = 0,
    maximum = 0
  )
  implicit class ValWithMaxLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, ValWithMax]) extends com.trueaccord.lenses.ObjectLens[UpperPB, ValWithMax](_l) {
    def current: com.trueaccord.lenses.Lens[UpperPB, Int] = field(_.current)((c_, f_) => c_.copy(current = f_))
    def maximum: com.trueaccord.lenses.Lens[UpperPB, Int] = field(_.maximum)((c_, f_) => c_.copy(maximum = f_))
  }
  final val CURRENT_FIELD_NUMBER = 1
  final val MAXIMUM_FIELD_NUMBER = 2
}
