// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.management


import com.trueaccord.scalapb.Descriptors

object InternalFields_management {
  def internalFieldsFor(scalaName: String): Seq[Descriptors.FieldDescriptor] = scalaName match {
    case "netmsg.management.FromClient" => Seq(Descriptors.FieldDescriptor(0, 1, "auto_register", Descriptors.Optional, Descriptors.MessageType(netmsg.management.AutoRegister.descriptor), isPacked = false, containingOneofName = None), Descriptors.FieldDescriptor(1, 2, "check_name_availability", Descriptors.Optional, Descriptors.MessageType(netmsg.management.CheckNameAvailability.descriptor), isPacked = false, containingOneofName = None), Descriptors.FieldDescriptor(2, 3, "register", Descriptors.Optional, Descriptors.MessageType(netmsg.management.Register.descriptor), isPacked = false, containingOneofName = None), Descriptors.FieldDescriptor(3, 4, "login", Descriptors.Optional, Descriptors.MessageType(netmsg.management.Login.descriptor), isPacked = false, containingOneofName = None), Descriptors.FieldDescriptor(4, 5, "join_game", Descriptors.Optional, Descriptors.MessageType(netmsg.management.JoinGame.descriptor), isPacked = false, containingOneofName = None), Descriptors.FieldDescriptor(5, 6, "cancel_join_game", Descriptors.Optional, Descriptors.MessageType(netmsg.management.CancelJoinGame.descriptor), isPacked = false, containingOneofName = None), Descriptors.FieldDescriptor(6, 7, "cancel_background_token", Descriptors.Optional, Descriptors.MessageType(netmsg.management.CancelBackgroundToken.descriptor), isPacked = false, containingOneofName = None))
    case "netmsg.management.FromServer" => Seq(Descriptors.FieldDescriptor(0, 1, "check_name_availability", Descriptors.Optional, Descriptors.MessageType(netmsg.management.CheckNameAvailabilityResponse.descriptor), isPacked = false, containingOneofName = None), Descriptors.FieldDescriptor(1, 2, "register", Descriptors.Optional, Descriptors.MessageType(netmsg.management.RegisterResponse.descriptor), isPacked = false, containingOneofName = None), Descriptors.FieldDescriptor(2, 3, "login", Descriptors.Optional, Descriptors.MessageType(netmsg.management.LoginResponse.descriptor), isPacked = false, containingOneofName = None), Descriptors.FieldDescriptor(3, 4, "game_joined", Descriptors.Optional, Descriptors.MessageType(netmsg.management.GameJoined.descriptor), isPacked = false, containingOneofName = None), Descriptors.FieldDescriptor(4, 5, "game_join_cancelled", Descriptors.Optional, Descriptors.MessageType(netmsg.management.JoinGameCancelled.descriptor), isPacked = false, containingOneofName = None), Descriptors.FieldDescriptor(5, 6, "waiting_list_joined", Descriptors.Optional, Descriptors.MessageType(netmsg.management.WaitingListJoined.descriptor), isPacked = false, containingOneofName = None))
    case "netmsg.management.Credentials" => Seq(Descriptors.FieldDescriptor(0, 1, "name", Descriptors.Required, Descriptors.PrimitiveType(com.google.protobuf.Descriptors.FieldDescriptor.JavaType.STRING, com.google.protobuf.Descriptors.FieldDescriptor.Type.STRING), isPacked = false, containingOneofName = None), Descriptors.FieldDescriptor(1, 2, "password", Descriptors.Optional, Descriptors.PrimitiveType(com.google.protobuf.Descriptors.FieldDescriptor.JavaType.STRING, com.google.protobuf.Descriptors.FieldDescriptor.Type.STRING), isPacked = false, containingOneofName = None), Descriptors.FieldDescriptor(2, 3, "session_token", Descriptors.Optional, Descriptors.PrimitiveType(com.google.protobuf.Descriptors.FieldDescriptor.JavaType.STRING, com.google.protobuf.Descriptors.FieldDescriptor.Type.STRING), isPacked = false, containingOneofName = None))
    case "netmsg.management.AutoRegister" => Seq()
    case "netmsg.management.CheckNameAvailability" => Seq(Descriptors.FieldDescriptor(0, 1, "name", Descriptors.Required, Descriptors.PrimitiveType(com.google.protobuf.Descriptors.FieldDescriptor.JavaType.STRING, com.google.protobuf.Descriptors.FieldDescriptor.Type.STRING), isPacked = false, containingOneofName = None))
    case "netmsg.management.CheckNameAvailabilityResponse" => Seq(Descriptors.FieldDescriptor(0, 1, "name", Descriptors.Required, Descriptors.PrimitiveType(com.google.protobuf.Descriptors.FieldDescriptor.JavaType.STRING, com.google.protobuf.Descriptors.FieldDescriptor.Type.STRING), isPacked = false, containingOneofName = None), Descriptors.FieldDescriptor(1, 2, "available", Descriptors.Required, Descriptors.PrimitiveType(com.google.protobuf.Descriptors.FieldDescriptor.JavaType.BOOLEAN, com.google.protobuf.Descriptors.FieldDescriptor.Type.BOOL), isPacked = false, containingOneofName = None))
    case "netmsg.management.Register" => Seq(Descriptors.FieldDescriptor(0, 1, "username", Descriptors.Required, Descriptors.PrimitiveType(com.google.protobuf.Descriptors.FieldDescriptor.JavaType.STRING, com.google.protobuf.Descriptors.FieldDescriptor.Type.STRING), isPacked = false, containingOneofName = None), Descriptors.FieldDescriptor(1, 2, "password", Descriptors.Required, Descriptors.PrimitiveType(com.google.protobuf.Descriptors.FieldDescriptor.JavaType.STRING, com.google.protobuf.Descriptors.FieldDescriptor.Type.STRING), isPacked = false, containingOneofName = None), Descriptors.FieldDescriptor(2, 3, "email", Descriptors.Required, Descriptors.PrimitiveType(com.google.protobuf.Descriptors.FieldDescriptor.JavaType.STRING, com.google.protobuf.Descriptors.FieldDescriptor.Type.STRING), isPacked = false, containingOneofName = None))
    case "netmsg.management.RegisterResponse" => Seq(Descriptors.FieldDescriptor(0, 1, "new_session_token", Descriptors.Optional, Descriptors.PrimitiveType(com.google.protobuf.Descriptors.FieldDescriptor.JavaType.STRING, com.google.protobuf.Descriptors.FieldDescriptor.Type.STRING), isPacked = false, containingOneofName = None))
    case "netmsg.management.Login" => Seq(Descriptors.FieldDescriptor(0, 1, "credentials", Descriptors.Required, Descriptors.MessageType(netmsg.management.Credentials.descriptor), isPacked = false, containingOneofName = None))
    case "netmsg.management.LoginResponse" => Seq(Descriptors.FieldDescriptor(0, 1, "data", Descriptors.Optional, Descriptors.MessageType(netmsg.management.LoginResponse.Data.descriptor), isPacked = false, containingOneofName = None))
    case "netmsg.management.LoginResponse.Data" => Seq(Descriptors.FieldDescriptor(0, 1, "id", Descriptors.Required, Descriptors.MessageType(netmsg.base.UUID.descriptor), isPacked = false, containingOneofName = None), Descriptors.FieldDescriptor(1, 2, "username", Descriptors.Required, Descriptors.PrimitiveType(com.google.protobuf.Descriptors.FieldDescriptor.JavaType.STRING, com.google.protobuf.Descriptors.FieldDescriptor.Type.STRING), isPacked = false, containingOneofName = None), Descriptors.FieldDescriptor(2, 3, "session_token", Descriptors.Required, Descriptors.PrimitiveType(com.google.protobuf.Descriptors.FieldDescriptor.JavaType.STRING, com.google.protobuf.Descriptors.FieldDescriptor.Type.STRING), isPacked = false, containingOneofName = None), Descriptors.FieldDescriptor(3, 4, "autogenerated", Descriptors.Required, Descriptors.PrimitiveType(com.google.protobuf.Descriptors.FieldDescriptor.JavaType.BOOLEAN, com.google.protobuf.Descriptors.FieldDescriptor.Type.BOOL), isPacked = false, containingOneofName = None))
    case "netmsg.management.JoinGame" => Seq(Descriptors.FieldDescriptor(0, 1, "mode", Descriptors.Required, Descriptors.MessageType(netmsg.management.JoinGame.Mode.descriptor), isPacked = false, containingOneofName = None))
    case "netmsg.management.JoinGame.Mode" => Seq(Descriptors.FieldDescriptor(0, 1, "teams", Descriptors.Required, Descriptors.PrimitiveType(com.google.protobuf.Descriptors.FieldDescriptor.JavaType.INT, com.google.protobuf.Descriptors.FieldDescriptor.Type.UINT32), isPacked = false, containingOneofName = None), Descriptors.FieldDescriptor(1, 2, "players_per_team", Descriptors.Required, Descriptors.PrimitiveType(com.google.protobuf.Descriptors.FieldDescriptor.JavaType.INT, com.google.protobuf.Descriptors.FieldDescriptor.Type.UINT32), isPacked = false, containingOneofName = None))
    case "netmsg.management.GameJoined" => Seq(Descriptors.FieldDescriptor(0, 1, "player", Descriptors.Required, Descriptors.MessageType(netmsg.game.Player.descriptor), isPacked = false, containingOneofName = None))
    case "netmsg.management.CancelJoinGame" => Seq()
    case "netmsg.management.JoinGameCancelled" => Seq()
    case "netmsg.management.CancelBackgroundToken" => Seq(Descriptors.FieldDescriptor(0, 1, "token", Descriptors.Required, Descriptors.PrimitiveType(com.google.protobuf.Descriptors.FieldDescriptor.JavaType.STRING, com.google.protobuf.Descriptors.FieldDescriptor.Type.STRING), isPacked = false, containingOneofName = None))
    case "netmsg.management.WaitingListJoined" => Seq(Descriptors.FieldDescriptor(0, 1, "background_token", Descriptors.Required, Descriptors.PrimitiveType(com.google.protobuf.Descriptors.FieldDescriptor.JavaType.STRING, com.google.protobuf.Descriptors.FieldDescriptor.Type.STRING), isPacked = false, containingOneofName = None))
  }
}