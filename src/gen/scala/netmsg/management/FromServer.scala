// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.management


import com.trueaccord.scalapb.Descriptors

final case class FromServer(
    checkNameAvailability: Option[netmsg.management.CheckNameAvailabilityResponse] = None,
    register: Option[netmsg.management.RegisterResponse] = None,
    login: Option[netmsg.management.LoginResponse] = None,
    gameJoined: Option[netmsg.management.GameJoined] = None,
    gameJoinCancelled: Option[netmsg.management.JoinGameCancelled] = None,
    waitingListJoined: Option[netmsg.management.WaitingListJoined] = None
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[FromServer] with com.trueaccord.lenses.Updatable[FromServer] {
    lazy val serializedSize: Int = {
      var __size = 0
      if (checkNameAvailability.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(checkNameAvailability.get.serializedSize) + checkNameAvailability.get.serializedSize }
      if (register.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(register.get.serializedSize) + register.get.serializedSize }
      if (login.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(login.get.serializedSize) + login.get.serializedSize }
      if (gameJoined.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(gameJoined.get.serializedSize) + gameJoined.get.serializedSize }
      if (gameJoinCancelled.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(gameJoinCancelled.get.serializedSize) + gameJoinCancelled.get.serializedSize }
      if (waitingListJoined.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(waitingListJoined.get.serializedSize) + waitingListJoined.get.serializedSize }
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      checkNameAvailability.foreach { v => 
        output.writeTag(1, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      register.foreach { v => 
        output.writeTag(2, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      login.foreach { v => 
        output.writeTag(3, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      gameJoined.foreach { v => 
        output.writeTag(4, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      gameJoinCancelled.foreach { v => 
        output.writeTag(5, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      waitingListJoined.foreach { v => 
        output.writeTag(6, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.management.FromServer = {
      var __checkNameAvailability = this.checkNameAvailability
      var __register = this.register
      var __login = this.login
      var __gameJoined = this.gameJoined
      var __gameJoinCancelled = this.gameJoinCancelled
      var __waitingListJoined = this.waitingListJoined
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __checkNameAvailability = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __checkNameAvailability.getOrElse(netmsg.management.CheckNameAvailabilityResponse.defaultInstance)))
          case 18 =>
            __register = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __register.getOrElse(netmsg.management.RegisterResponse.defaultInstance)))
          case 26 =>
            __login = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __login.getOrElse(netmsg.management.LoginResponse.defaultInstance)))
          case 34 =>
            __gameJoined = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __gameJoined.getOrElse(netmsg.management.GameJoined.defaultInstance)))
          case 42 =>
            __gameJoinCancelled = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __gameJoinCancelled.getOrElse(netmsg.management.JoinGameCancelled.defaultInstance)))
          case 50 =>
            __waitingListJoined = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __waitingListJoined.getOrElse(netmsg.management.WaitingListJoined.defaultInstance)))
          case tag => __input.skipField(tag)
        }
      }
      netmsg.management.FromServer(
          checkNameAvailability = __checkNameAvailability,
          register = __register,
          login = __login,
          gameJoined = __gameJoined,
          gameJoinCancelled = __gameJoinCancelled,
          waitingListJoined = __waitingListJoined
      )
    }
    def getCheckNameAvailability: netmsg.management.CheckNameAvailabilityResponse = checkNameAvailability.getOrElse(netmsg.management.CheckNameAvailabilityResponse.defaultInstance)
    def clearCheckNameAvailability: FromServer = copy(checkNameAvailability = None)
    def withCheckNameAvailability(__v: netmsg.management.CheckNameAvailabilityResponse): FromServer = copy(checkNameAvailability = Some(__v))
    def getRegister: netmsg.management.RegisterResponse = register.getOrElse(netmsg.management.RegisterResponse.defaultInstance)
    def clearRegister: FromServer = copy(register = None)
    def withRegister(__v: netmsg.management.RegisterResponse): FromServer = copy(register = Some(__v))
    def getLogin: netmsg.management.LoginResponse = login.getOrElse(netmsg.management.LoginResponse.defaultInstance)
    def clearLogin: FromServer = copy(login = None)
    def withLogin(__v: netmsg.management.LoginResponse): FromServer = copy(login = Some(__v))
    def getGameJoined: netmsg.management.GameJoined = gameJoined.getOrElse(netmsg.management.GameJoined.defaultInstance)
    def clearGameJoined: FromServer = copy(gameJoined = None)
    def withGameJoined(__v: netmsg.management.GameJoined): FromServer = copy(gameJoined = Some(__v))
    def getGameJoinCancelled: netmsg.management.JoinGameCancelled = gameJoinCancelled.getOrElse(netmsg.management.JoinGameCancelled.defaultInstance)
    def clearGameJoinCancelled: FromServer = copy(gameJoinCancelled = None)
    def withGameJoinCancelled(__v: netmsg.management.JoinGameCancelled): FromServer = copy(gameJoinCancelled = Some(__v))
    def getWaitingListJoined: netmsg.management.WaitingListJoined = waitingListJoined.getOrElse(netmsg.management.WaitingListJoined.defaultInstance)
    def clearWaitingListJoined: FromServer = copy(waitingListJoined = None)
    def withWaitingListJoined(__v: netmsg.management.WaitingListJoined): FromServer = copy(waitingListJoined = Some(__v))
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => checkNameAvailability
        case 2 => register
        case 3 => login
        case 4 => gameJoined
        case 5 => gameJoinCancelled
        case 6 => waitingListJoined
      }
    }
    def companion = netmsg.management.FromServer
}

object FromServer extends com.trueaccord.scalapb.GeneratedMessageCompanion[FromServer]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[FromServer]  = this
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.management.FromServer = netmsg.management.FromServer(
    checkNameAvailability = fieldsMap.getOrElse(1, None).asInstanceOf[Option[netmsg.management.CheckNameAvailabilityResponse]],
    register = fieldsMap.getOrElse(2, None).asInstanceOf[Option[netmsg.management.RegisterResponse]],
    login = fieldsMap.getOrElse(3, None).asInstanceOf[Option[netmsg.management.LoginResponse]],
    gameJoined = fieldsMap.getOrElse(4, None).asInstanceOf[Option[netmsg.management.GameJoined]],
    gameJoinCancelled = fieldsMap.getOrElse(5, None).asInstanceOf[Option[netmsg.management.JoinGameCancelled]],
    waitingListJoined = fieldsMap.getOrElse(6, None).asInstanceOf[Option[netmsg.management.WaitingListJoined]]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("FromServer", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.management.InternalFields_management.internalFieldsFor("netmsg.management.FromServer"))
  lazy val defaultInstance = netmsg.management.FromServer(
  )
  implicit class FromServerLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, FromServer]) extends com.trueaccord.lenses.ObjectLens[UpperPB, FromServer](_l) {
    def checkNameAvailability: com.trueaccord.lenses.Lens[UpperPB, netmsg.management.CheckNameAvailabilityResponse] = field(_.getCheckNameAvailability)((c_, f_) => c_.copy(checkNameAvailability = Some(f_)))
    def optionalCheckNameAvailability: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.management.CheckNameAvailabilityResponse]] = field(_.checkNameAvailability)((c_, f_) => c_.copy(checkNameAvailability = f_))
    def register: com.trueaccord.lenses.Lens[UpperPB, netmsg.management.RegisterResponse] = field(_.getRegister)((c_, f_) => c_.copy(register = Some(f_)))
    def optionalRegister: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.management.RegisterResponse]] = field(_.register)((c_, f_) => c_.copy(register = f_))
    def login: com.trueaccord.lenses.Lens[UpperPB, netmsg.management.LoginResponse] = field(_.getLogin)((c_, f_) => c_.copy(login = Some(f_)))
    def optionalLogin: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.management.LoginResponse]] = field(_.login)((c_, f_) => c_.copy(login = f_))
    def gameJoined: com.trueaccord.lenses.Lens[UpperPB, netmsg.management.GameJoined] = field(_.getGameJoined)((c_, f_) => c_.copy(gameJoined = Some(f_)))
    def optionalGameJoined: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.management.GameJoined]] = field(_.gameJoined)((c_, f_) => c_.copy(gameJoined = f_))
    def gameJoinCancelled: com.trueaccord.lenses.Lens[UpperPB, netmsg.management.JoinGameCancelled] = field(_.getGameJoinCancelled)((c_, f_) => c_.copy(gameJoinCancelled = Some(f_)))
    def optionalGameJoinCancelled: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.management.JoinGameCancelled]] = field(_.gameJoinCancelled)((c_, f_) => c_.copy(gameJoinCancelled = f_))
    def waitingListJoined: com.trueaccord.lenses.Lens[UpperPB, netmsg.management.WaitingListJoined] = field(_.getWaitingListJoined)((c_, f_) => c_.copy(waitingListJoined = Some(f_)))
    def optionalWaitingListJoined: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.management.WaitingListJoined]] = field(_.waitingListJoined)((c_, f_) => c_.copy(waitingListJoined = f_))
  }
  final val CHECK_NAME_AVAILABILITY_FIELD_NUMBER = 1
  final val REGISTER_FIELD_NUMBER = 2
  final val LOGIN_FIELD_NUMBER = 3
  final val GAME_JOINED_FIELD_NUMBER = 4
  final val GAME_JOIN_CANCELLED_FIELD_NUMBER = 5
  final val WAITING_LIST_JOINED_FIELD_NUMBER = 6
}
