// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.bg_client


import com.trueaccord.scalapb.Descriptors

final case class Client2Server(
    login: Option[netmsg.bg_client.Login] = None
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[Client2Server] with com.trueaccord.lenses.Updatable[Client2Server] {
    lazy val serializedSize: Int = {
      var __size = 0
      if (login.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(login.get.serializedSize) + login.get.serializedSize }
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      login.foreach { v => 
        output.writeTag(1, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.bg_client.Client2Server = {
      var __login = this.login
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __login = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __login.getOrElse(netmsg.bg_client.Login.defaultInstance)))
          case tag => __input.skipField(tag)
        }
      }
      netmsg.bg_client.Client2Server(
          login = __login
      )
    }
    def getLogin: netmsg.bg_client.Login = login.getOrElse(netmsg.bg_client.Login.defaultInstance)
    def clearLogin: Client2Server = copy(login = None)
    def withLogin(__v: netmsg.bg_client.Login): Client2Server = copy(login = Some(__v))
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => login
      }
    }
    def companion = netmsg.bg_client.Client2Server
}

object Client2Server extends com.trueaccord.scalapb.GeneratedMessageCompanion[Client2Server]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[Client2Server]  = this
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.bg_client.Client2Server = netmsg.bg_client.Client2Server(
    login = fieldsMap.getOrElse(1, None).asInstanceOf[Option[netmsg.bg_client.Login]]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("Client2Server", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.bg_client.InternalFields_bg_client.internalFieldsFor("netmsg.bg_client.Client2Server"))
  lazy val defaultInstance = netmsg.bg_client.Client2Server(
  )
  implicit class Client2ServerLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, Client2Server]) extends com.trueaccord.lenses.ObjectLens[UpperPB, Client2Server](_l) {
    def login: com.trueaccord.lenses.Lens[UpperPB, netmsg.bg_client.Login] = field(_.getLogin)((c_, f_) => c_.copy(login = Some(f_)))
    def optionalLogin: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.bg_client.Login]] = field(_.login)((c_, f_) => c_.copy(login = f_))
  }
  final val LOGIN_FIELD_NUMBER = 1
}
