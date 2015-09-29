// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.game


import com.trueaccord.scalapb.Descriptors

final case class MJoined(
    player: netmsg.game.Player
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[MJoined] with com.trueaccord.lenses.Updatable[MJoined] {
    lazy val serializedSize: Int = {
      var __size = 0
      __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(player.serializedSize) + player.serializedSize
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      output.writeTag(1, 2)
      output.writeRawVarint32(player.serializedSize)
      player.writeTo(output)
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.game.MJoined = {
      var __player = this.player
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __player = com.trueaccord.scalapb.LiteParser.readMessage(__input, __player)
          case tag => __input.skipField(tag)
        }
      }
      netmsg.game.MJoined(
          player = __player
      )
    }
    def withPlayer(__v: netmsg.game.Player): MJoined = copy(player = __v)
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => player
      }
    }
    def companion = netmsg.game.MJoined
}

object MJoined extends com.trueaccord.scalapb.GeneratedMessageCompanion[MJoined]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[MJoined]  = this
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.game.MJoined = netmsg.game.MJoined(
    player = fieldsMap(1).asInstanceOf[netmsg.game.Player]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("MJoined", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.game.InternalFields_game.internalFieldsFor("netmsg.game.MJoined"))
  lazy val defaultInstance = netmsg.game.MJoined(
    player = netmsg.game.Player.defaultInstance
  )
  implicit class MJoinedLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, MJoined]) extends com.trueaccord.lenses.ObjectLens[UpperPB, MJoined](_l) {
    def player: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.Player] = field(_.player)((c_, f_) => c_.copy(player = f_))
  }
  final val PLAYER_FIELD_NUMBER = 1
}