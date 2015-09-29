// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.game


import com.trueaccord.scalapb.Descriptors

final case class PointOwnerMapChangeEvt(
    kind: netmsg.game.PointOwnerMapChangeEvt.Kind,
    owned: Seq[netmsg.base.Vect2] = Nil,
    unowned: Seq[netmsg.base.Vect2] = Nil
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[PointOwnerMapChangeEvt] with com.trueaccord.lenses.Updatable[PointOwnerMapChangeEvt] {
    lazy val serializedSize: Int = {
      var __size = 0
      __size += com.google.protobuf.CodedOutputStream.computeEnumSize(1, kind.id)
      owned.foreach(owned => __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(owned.serializedSize) + owned.serializedSize)
      unowned.foreach(unowned => __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(unowned.serializedSize) + unowned.serializedSize)
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      output.writeEnum(1, kind.id)
      owned.foreach { v => 
        output.writeTag(2, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      unowned.foreach { v => 
        output.writeTag(3, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.game.PointOwnerMapChangeEvt = {
      var __kind = this.kind
      val __owned = (scala.collection.immutable.Vector.newBuilder[netmsg.base.Vect2] ++= this.owned)
      val __unowned = (scala.collection.immutable.Vector.newBuilder[netmsg.base.Vect2] ++= this.unowned)
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 8 =>
            __kind = netmsg.game.PointOwnerMapChangeEvt.Kind.fromValue(__input.readEnum())
          case 18 =>
            __owned += com.trueaccord.scalapb.LiteParser.readMessage(__input, netmsg.base.Vect2.defaultInstance)
          case 26 =>
            __unowned += com.trueaccord.scalapb.LiteParser.readMessage(__input, netmsg.base.Vect2.defaultInstance)
          case tag => __input.skipField(tag)
        }
      }
      netmsg.game.PointOwnerMapChangeEvt(
          kind = __kind,
          owned = __owned.result(),
          unowned = __unowned.result()
      )
    }
    def withKind(__v: netmsg.game.PointOwnerMapChangeEvt.Kind): PointOwnerMapChangeEvt = copy(kind = __v)
    def clearOwned = copy(owned = Nil)
    def addOwned(__vs: netmsg.base.Vect2*): PointOwnerMapChangeEvt = addAllOwned(__vs)
    def addAllOwned(__vs: TraversableOnce[netmsg.base.Vect2]): PointOwnerMapChangeEvt = copy(owned = owned ++ __vs)
    def withOwned(__v: Seq[netmsg.base.Vect2]): PointOwnerMapChangeEvt = copy(owned = __v)
    def clearUnowned = copy(unowned = Nil)
    def addUnowned(__vs: netmsg.base.Vect2*): PointOwnerMapChangeEvt = addAllUnowned(__vs)
    def addAllUnowned(__vs: TraversableOnce[netmsg.base.Vect2]): PointOwnerMapChangeEvt = copy(unowned = unowned ++ __vs)
    def withUnowned(__v: Seq[netmsg.base.Vect2]): PointOwnerMapChangeEvt = copy(unowned = __v)
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => kind
        case 2 => owned
        case 3 => unowned
      }
    }
    def companion = netmsg.game.PointOwnerMapChangeEvt
}

object PointOwnerMapChangeEvt extends com.trueaccord.scalapb.GeneratedMessageCompanion[PointOwnerMapChangeEvt]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[PointOwnerMapChangeEvt]  = this
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.game.PointOwnerMapChangeEvt = netmsg.game.PointOwnerMapChangeEvt(
    kind = fieldsMap(1).asInstanceOf[netmsg.game.PointOwnerMapChangeEvt.Kind],
    owned = fieldsMap.getOrElse(2, Nil).asInstanceOf[Seq[netmsg.base.Vect2]],
    unowned = fieldsMap.getOrElse(3, Nil).asInstanceOf[Seq[netmsg.base.Vect2]]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("PointOwnerMapChangeEvt", this,
    None, m = Seq(),
    e = Seq(netmsg.game.PointOwnerMapChangeEvt.Kind.descriptor),
    f = netmsg.game.InternalFields_game.internalFieldsFor("netmsg.game.PointOwnerMapChangeEvt"))
  lazy val defaultInstance = netmsg.game.PointOwnerMapChangeEvt(
    kind = netmsg.game.PointOwnerMapChangeEvt.Kind.WARP_ZONE
  )
  sealed trait Kind extends com.trueaccord.scalapb.GeneratedEnum {
    def isWarpZone: Boolean = false
    def isVisibility: Boolean = false
  }
  
  object Kind extends com.trueaccord.scalapb.GeneratedEnumCompanion[Kind] {
    case object WARP_ZONE extends Kind {
      val id = 1
      val name = "WARP_ZONE"
      override def isWarpZone: Boolean = true
    }
    
    case object VISIBILITY extends Kind {
      val id = 2
      val name = "VISIBILITY"
      override def isVisibility: Boolean = true
    }
    
    lazy val values = Seq(WARP_ZONE, VISIBILITY)
    def fromValue(value: Int): Kind = value match {
      case 1 => WARP_ZONE
      case 2 => VISIBILITY
    }
    lazy val descriptor = new Descriptors.EnumDescriptor(0, "Kind", this)
  }
  implicit class PointOwnerMapChangeEvtLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, PointOwnerMapChangeEvt]) extends com.trueaccord.lenses.ObjectLens[UpperPB, PointOwnerMapChangeEvt](_l) {
    def kind: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.PointOwnerMapChangeEvt.Kind] = field(_.kind)((c_, f_) => c_.copy(kind = f_))
    def owned: com.trueaccord.lenses.Lens[UpperPB, Seq[netmsg.base.Vect2]] = field(_.owned)((c_, f_) => c_.copy(owned = f_))
    def unowned: com.trueaccord.lenses.Lens[UpperPB, Seq[netmsg.base.Vect2]] = field(_.unowned)((c_, f_) => c_.copy(unowned = f_))
  }
  final val KIND_FIELD_NUMBER = 1
  final val OWNED_FIELD_NUMBER = 2
  final val UNOWNED_FIELD_NUMBER = 3
}