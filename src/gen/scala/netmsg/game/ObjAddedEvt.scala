// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.game


import com.trueaccord.scalapb.Descriptors

final case class ObjAddedEvt(
    `object`: netmsg.game.WObject,
    reason: netmsg.game.ObjAddedEvt.Reason
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[ObjAddedEvt] with com.trueaccord.lenses.Updatable[ObjAddedEvt] {
    lazy val serializedSize: Int = {
      var __size = 0
      __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(`object`.serializedSize) + `object`.serializedSize
      __size += com.google.protobuf.CodedOutputStream.computeEnumSize(2, reason.id)
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      output.writeTag(1, 2)
      output.writeRawVarint32(`object`.serializedSize)
      `object`.writeTo(output)
      output.writeEnum(2, reason.id)
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.game.ObjAddedEvt = {
      var __object = this.`object`
      var __reason = this.reason
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __object = com.trueaccord.scalapb.LiteParser.readMessage(__input, __object)
          case 16 =>
            __reason = netmsg.game.ObjAddedEvt.Reason.fromValue(__input.readEnum())
          case tag => __input.skipField(tag)
        }
      }
      netmsg.game.ObjAddedEvt(
          `object` = __object,
          reason = __reason
      )
    }
    def withObject(__v: netmsg.game.WObject): ObjAddedEvt = copy(`object` = __v)
    def withReason(__v: netmsg.game.ObjAddedEvt.Reason): ObjAddedEvt = copy(reason = __v)
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => `object`
        case 2 => reason
      }
    }
    def companion = netmsg.game.ObjAddedEvt
}

object ObjAddedEvt extends com.trueaccord.scalapb.GeneratedMessageCompanion[ObjAddedEvt]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[ObjAddedEvt]  = this
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.game.ObjAddedEvt = netmsg.game.ObjAddedEvt(
    `object` = fieldsMap(1).asInstanceOf[netmsg.game.WObject],
    reason = fieldsMap(2).asInstanceOf[netmsg.game.ObjAddedEvt.Reason]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("ObjAddedEvt", this,
    None, m = Seq(),
    e = Seq(netmsg.game.ObjAddedEvt.Reason.descriptor),
    f = netmsg.game.InternalFields_game.internalFieldsFor("netmsg.game.ObjAddedEvt"))
  lazy val defaultInstance = netmsg.game.ObjAddedEvt(
    `object` = netmsg.game.WObject.defaultInstance,
    reason = netmsg.game.ObjAddedEvt.Reason.DEFAULT
  )
  sealed trait Reason extends com.trueaccord.scalapb.GeneratedEnum {
    def isDefault: Boolean = false
    def isDeployment: Boolean = false
  }
  
  object Reason extends com.trueaccord.scalapb.GeneratedEnumCompanion[Reason] {
    case object DEFAULT extends Reason {
      val id = 1
      val name = "DEFAULT"
      override def isDefault: Boolean = true
    }
    
    case object DEPLOYMENT extends Reason {
      val id = 2
      val name = "DEPLOYMENT"
      override def isDeployment: Boolean = true
    }
    
    lazy val values = Seq(DEFAULT, DEPLOYMENT)
    def fromValue(value: Int): Reason = value match {
      case 1 => DEFAULT
      case 2 => DEPLOYMENT
    }
    lazy val descriptor = new Descriptors.EnumDescriptor(0, "Reason", this)
  }
  implicit class ObjAddedEvtLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, ObjAddedEvt]) extends com.trueaccord.lenses.ObjectLens[UpperPB, ObjAddedEvt](_l) {
    def `object`: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObject] = field(_.`object`)((c_, f_) => c_.copy(`object` = f_))
    def reason: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.ObjAddedEvt.Reason] = field(_.reason)((c_, f_) => c_.copy(reason = f_))
  }
  final val OBJECT_FIELD_NUMBER = 1
  final val REASON_FIELD_NUMBER = 2
}
