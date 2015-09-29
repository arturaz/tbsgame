// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.game


import com.trueaccord.scalapb.Descriptors

final case class GameWonEvt(
    teamId: netmsg.game.TeamID
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[GameWonEvt] with com.trueaccord.lenses.Updatable[GameWonEvt] {
    lazy val serializedSize: Int = {
      var __size = 0
      __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(teamId.serializedSize) + teamId.serializedSize
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      output.writeTag(1, 2)
      output.writeRawVarint32(teamId.serializedSize)
      teamId.writeTo(output)
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.game.GameWonEvt = {
      var __teamId = this.teamId
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __teamId = com.trueaccord.scalapb.LiteParser.readMessage(__input, __teamId)
          case tag => __input.skipField(tag)
        }
      }
      netmsg.game.GameWonEvt(
          teamId = __teamId
      )
    }
    def withTeamId(__v: netmsg.game.TeamID): GameWonEvt = copy(teamId = __v)
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => teamId
      }
    }
    def companion = netmsg.game.GameWonEvt
}

object GameWonEvt extends com.trueaccord.scalapb.GeneratedMessageCompanion[GameWonEvt]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[GameWonEvt]  = this
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.game.GameWonEvt = netmsg.game.GameWonEvt(
    teamId = fieldsMap(1).asInstanceOf[netmsg.game.TeamID]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("GameWonEvt", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.game.InternalFields_game.internalFieldsFor("netmsg.game.GameWonEvt"))
  lazy val defaultInstance = netmsg.game.GameWonEvt(
    teamId = netmsg.game.TeamID.defaultInstance
  )
  implicit class GameWonEvtLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, GameWonEvt]) extends com.trueaccord.lenses.ObjectLens[UpperPB, GameWonEvt](_l) {
    def teamId: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.TeamID] = field(_.teamId)((c_, f_) => c_.copy(teamId = f_))
  }
  final val TEAM_ID_FIELD_NUMBER = 1
}