// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.management


import com.trueaccord.scalapb.Descriptors

final case class CancelJoinGame(
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[CancelJoinGame] with com.trueaccord.lenses.Updatable[CancelJoinGame] {
    lazy val serializedSize: Int = {
      var __size = 0
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.management.CancelJoinGame = {
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case tag => __input.skipField(tag)
        }
      }
      netmsg.management.CancelJoinGame(
      )
    }
    def getField(__field: Descriptors.FieldDescriptor): Any = throw new MatchError(__field)
    def companion = netmsg.management.CancelJoinGame
}

object CancelJoinGame extends com.trueaccord.scalapb.GeneratedMessageCompanion[CancelJoinGame]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[CancelJoinGame]  = this
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.management.CancelJoinGame = netmsg.management.CancelJoinGame(
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("CancelJoinGame", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.management.InternalFields_management.internalFieldsFor("netmsg.management.CancelJoinGame"))
  lazy val defaultInstance = netmsg.management.CancelJoinGame(
  )
  implicit class CancelJoinGameLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, CancelJoinGame]) extends com.trueaccord.lenses.ObjectLens[UpperPB, CancelJoinGame](_l) {
  }
}