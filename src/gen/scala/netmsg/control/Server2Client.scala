// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.control


import com.trueaccord.scalapb.Descriptors

final case class Server2Client(
    reply: Option[netmsg.control.GenericReply] = None,
    status: Option[netmsg.control.StatusReply] = None
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[Server2Client] with com.trueaccord.lenses.Updatable[Server2Client] {
    lazy val serializedSize: Int = {
      var __size = 0
      if (reply.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(reply.get.serializedSize) + reply.get.serializedSize }
      if (status.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(status.get.serializedSize) + status.get.serializedSize }
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      reply.foreach { v => 
        output.writeTag(1, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      status.foreach { v => 
        output.writeTag(2, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.control.Server2Client = {
      var __reply = this.reply
      var __status = this.status
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __reply = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __reply.getOrElse(netmsg.control.GenericReply.defaultInstance)))
          case 18 =>
            __status = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __status.getOrElse(netmsg.control.StatusReply.defaultInstance)))
          case tag => __input.skipField(tag)
        }
      }
      netmsg.control.Server2Client(
          reply = __reply,
          status = __status
      )
    }
    def getReply: netmsg.control.GenericReply = reply.getOrElse(netmsg.control.GenericReply.defaultInstance)
    def clearReply: Server2Client = copy(reply = None)
    def withReply(__v: netmsg.control.GenericReply): Server2Client = copy(reply = Some(__v))
    def getStatus: netmsg.control.StatusReply = status.getOrElse(netmsg.control.StatusReply.defaultInstance)
    def clearStatus: Server2Client = copy(status = None)
    def withStatus(__v: netmsg.control.StatusReply): Server2Client = copy(status = Some(__v))
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => reply
        case 2 => status
      }
    }
    def companion = netmsg.control.Server2Client
}

object Server2Client extends com.trueaccord.scalapb.GeneratedMessageCompanion[Server2Client]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[Server2Client]  = this
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.control.Server2Client = netmsg.control.Server2Client(
    reply = fieldsMap.getOrElse(1, None).asInstanceOf[Option[netmsg.control.GenericReply]],
    status = fieldsMap.getOrElse(2, None).asInstanceOf[Option[netmsg.control.StatusReply]]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("Server2Client", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.control.InternalFields_control.internalFieldsFor("netmsg.control.Server2Client"))
  lazy val defaultInstance = netmsg.control.Server2Client(
  )
  implicit class Server2ClientLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, Server2Client]) extends com.trueaccord.lenses.ObjectLens[UpperPB, Server2Client](_l) {
    def reply: com.trueaccord.lenses.Lens[UpperPB, netmsg.control.GenericReply] = field(_.getReply)((c_, f_) => c_.copy(reply = Some(f_)))
    def optionalReply: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.control.GenericReply]] = field(_.reply)((c_, f_) => c_.copy(reply = f_))
    def status: com.trueaccord.lenses.Lens[UpperPB, netmsg.control.StatusReply] = field(_.getStatus)((c_, f_) => c_.copy(status = Some(f_)))
    def optionalStatus: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.control.StatusReply]] = field(_.status)((c_, f_) => c_.copy(status = f_))
  }
  final val REPLY_FIELD_NUMBER = 1
  final val STATUS_FIELD_NUMBER = 2
}
