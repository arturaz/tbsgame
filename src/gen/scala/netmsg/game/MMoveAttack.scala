// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.game


import com.trueaccord.scalapb.Descriptors

final case class MMoveAttack(
    id: netmsg.game.WObjID,
    path: Seq[netmsg.base.Vect2] = Nil,
    targetId: netmsg.game.WObjID
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[MMoveAttack] with com.trueaccord.lenses.Updatable[MMoveAttack] {
    lazy val serializedSize: Int = {
      var __size = 0
      __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(id.serializedSize) + id.serializedSize
      path.foreach(path => __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(path.serializedSize) + path.serializedSize)
      __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(targetId.serializedSize) + targetId.serializedSize
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      output.writeTag(1, 2)
      output.writeRawVarint32(id.serializedSize)
      id.writeTo(output)
      path.foreach { v => 
        output.writeTag(2, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      output.writeTag(3, 2)
      output.writeRawVarint32(targetId.serializedSize)
      targetId.writeTo(output)
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.game.MMoveAttack = {
      var __id = this.id
      val __path = (scala.collection.immutable.Vector.newBuilder[netmsg.base.Vect2] ++= this.path)
      var __targetId = this.targetId
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __id = com.trueaccord.scalapb.LiteParser.readMessage(__input, __id)
          case 18 =>
            __path += com.trueaccord.scalapb.LiteParser.readMessage(__input, netmsg.base.Vect2.defaultInstance)
          case 26 =>
            __targetId = com.trueaccord.scalapb.LiteParser.readMessage(__input, __targetId)
          case tag => __input.skipField(tag)
        }
      }
      netmsg.game.MMoveAttack(
          id = __id,
          path = __path.result(),
          targetId = __targetId
      )
    }
    def withId(__v: netmsg.game.WObjID): MMoveAttack = copy(id = __v)
    def clearPath = copy(path = Nil)
    def addPath(__vs: netmsg.base.Vect2*): MMoveAttack = addAllPath(__vs)
    def addAllPath(__vs: TraversableOnce[netmsg.base.Vect2]): MMoveAttack = copy(path = path ++ __vs)
    def withPath(__v: Seq[netmsg.base.Vect2]): MMoveAttack = copy(path = __v)
    def withTargetId(__v: netmsg.game.WObjID): MMoveAttack = copy(targetId = __v)
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => id
        case 2 => path
        case 3 => targetId
      }
    }
    def companion = netmsg.game.MMoveAttack
}

object MMoveAttack extends com.trueaccord.scalapb.GeneratedMessageCompanion[MMoveAttack]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[MMoveAttack]  = this
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.game.MMoveAttack = netmsg.game.MMoveAttack(
    id = fieldsMap(1).asInstanceOf[netmsg.game.WObjID],
    path = fieldsMap.getOrElse(2, Nil).asInstanceOf[Seq[netmsg.base.Vect2]],
    targetId = fieldsMap(3).asInstanceOf[netmsg.game.WObjID]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("MMoveAttack", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.game.InternalFields_game.internalFieldsFor("netmsg.game.MMoveAttack"))
  lazy val defaultInstance = netmsg.game.MMoveAttack(
    id = netmsg.game.WObjID.defaultInstance,
    targetId = netmsg.game.WObjID.defaultInstance
  )
  implicit class MMoveAttackLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, MMoveAttack]) extends com.trueaccord.lenses.ObjectLens[UpperPB, MMoveAttack](_l) {
    def id: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObjID] = field(_.id)((c_, f_) => c_.copy(id = f_))
    def path: com.trueaccord.lenses.Lens[UpperPB, Seq[netmsg.base.Vect2]] = field(_.path)((c_, f_) => c_.copy(path = f_))
    def targetId: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObjID] = field(_.targetId)((c_, f_) => c_.copy(targetId = f_))
  }
  final val ID_FIELD_NUMBER = 1
  final val PATH_FIELD_NUMBER = 2
  final val TARGET_ID_FIELD_NUMBER = 3
}