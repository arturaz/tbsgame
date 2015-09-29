// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.game


import com.trueaccord.scalapb.Descriptors

final case class JoinEvt(
    player: netmsg.game.InitPlayer
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[JoinEvt] with com.trueaccord.lenses.Updatable[JoinEvt] {
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
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.game.JoinEvt = {
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
      netmsg.game.JoinEvt(
          player = __player
      )
    }
    def withPlayer(__v: netmsg.game.InitPlayer): JoinEvt = copy(player = __v)
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => player
      }
    }
    def companion = netmsg.game.JoinEvt
}

object JoinEvt extends com.trueaccord.scalapb.GeneratedMessageCompanion[JoinEvt]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[JoinEvt]  = this
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.game.JoinEvt = netmsg.game.JoinEvt(
    player = fieldsMap(1).asInstanceOf[netmsg.game.InitPlayer]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("JoinEvt", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.game.InternalFields_game.internalFieldsFor("netmsg.game.JoinEvt"))
  lazy val defaultInstance = netmsg.game.JoinEvt(
    player = netmsg.game.InitPlayer.defaultInstance
  )
  implicit class JoinEvtLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, JoinEvt]) extends com.trueaccord.lenses.ObjectLens[UpperPB, JoinEvt](_l) {
    def player: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.InitPlayer] = field(_.player)((c_, f_) => c_.copy(player = f_))
  }
  final val PLAYER_FIELD_NUMBER = 1
}