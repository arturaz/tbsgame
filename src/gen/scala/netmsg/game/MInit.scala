// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.game

import scala.collection.JavaConversions._
import com.trueaccord.scalapb.Descriptors

final case class MInit(
    bounds: netmsg.base.Bounds,
    objects: Seq[netmsg.game.WObject] = Nil,
    warpZone: Seq[netmsg.base.Vect2] = Nil,
    visiblePoints: Seq[netmsg.base.Vect2] = Nil,
    selfTeam: netmsg.game.Team,
    otherTeams: Seq[netmsg.game.Team] = Nil,
    self: netmsg.game.PlayerState,
    otherPlayers: Seq[netmsg.game.InitPlayer] = Nil,
    warpableObjectStats: netmsg.game.WarpableObjectStats,
    attackMultipliers: Seq[netmsg.game.MInit.AttackMultiplier] = Nil,
    objectives: netmsg.game.Objectives,
    turnTimeframe: Option[netmsg.base.Timeframe] = None,
    extractionSpeedRates: Seq[netmsg.game.MInit.ExtractionSpeedRate] = Nil
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[MInit] with com.trueaccord.lenses.Updatable[MInit] {
    lazy val serializedSize: Int = {
      var __size = 0
      __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(bounds.serializedSize) + bounds.serializedSize
      objects.foreach(objects => __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(objects.serializedSize) + objects.serializedSize)
      warpZone.foreach(warpZone => __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(warpZone.serializedSize) + warpZone.serializedSize)
      visiblePoints.foreach(visiblePoints => __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(visiblePoints.serializedSize) + visiblePoints.serializedSize)
      __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(selfTeam.serializedSize) + selfTeam.serializedSize
      otherTeams.foreach(otherTeams => __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(otherTeams.serializedSize) + otherTeams.serializedSize)
      __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(self.serializedSize) + self.serializedSize
      otherPlayers.foreach(otherPlayers => __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(otherPlayers.serializedSize) + otherPlayers.serializedSize)
      __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(warpableObjectStats.serializedSize) + warpableObjectStats.serializedSize
      attackMultipliers.foreach(attackMultipliers => __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(attackMultipliers.serializedSize) + attackMultipliers.serializedSize)
      __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(objectives.serializedSize) + objectives.serializedSize
      if (turnTimeframe.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(turnTimeframe.get.serializedSize) + turnTimeframe.get.serializedSize }
      extractionSpeedRates.foreach(extractionSpeedRates => __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(extractionSpeedRates.serializedSize) + extractionSpeedRates.serializedSize)
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      output.writeTag(1, 2)
      output.writeRawVarint32(bounds.serializedSize)
      bounds.writeTo(output)
      objects.foreach { v => 
        output.writeTag(2, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      warpZone.foreach { v => 
        output.writeTag(3, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      visiblePoints.foreach { v => 
        output.writeTag(4, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      output.writeTag(5, 2)
      output.writeRawVarint32(selfTeam.serializedSize)
      selfTeam.writeTo(output)
      otherTeams.foreach { v => 
        output.writeTag(6, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      output.writeTag(7, 2)
      output.writeRawVarint32(self.serializedSize)
      self.writeTo(output)
      otherPlayers.foreach { v => 
        output.writeTag(8, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      output.writeTag(9, 2)
      output.writeRawVarint32(warpableObjectStats.serializedSize)
      warpableObjectStats.writeTo(output)
      attackMultipliers.foreach { v => 
        output.writeTag(10, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      output.writeTag(11, 2)
      output.writeRawVarint32(objectives.serializedSize)
      objectives.writeTo(output)
      turnTimeframe.foreach { v => 
        output.writeTag(12, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      extractionSpeedRates.foreach { v => 
        output.writeTag(13, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.game.MInit = {
      var __bounds = this.bounds
      val __objects = (scala.collection.immutable.Vector.newBuilder[netmsg.game.WObject] ++= this.objects)
      val __warpZone = (scala.collection.immutable.Vector.newBuilder[netmsg.base.Vect2] ++= this.warpZone)
      val __visiblePoints = (scala.collection.immutable.Vector.newBuilder[netmsg.base.Vect2] ++= this.visiblePoints)
      var __selfTeam = this.selfTeam
      val __otherTeams = (scala.collection.immutable.Vector.newBuilder[netmsg.game.Team] ++= this.otherTeams)
      var __self = this.self
      val __otherPlayers = (scala.collection.immutable.Vector.newBuilder[netmsg.game.InitPlayer] ++= this.otherPlayers)
      var __warpableObjectStats = this.warpableObjectStats
      val __attackMultipliers = (scala.collection.immutable.Vector.newBuilder[netmsg.game.MInit.AttackMultiplier] ++= this.attackMultipliers)
      var __objectives = this.objectives
      var __turnTimeframe = this.turnTimeframe
      val __extractionSpeedRates = (scala.collection.immutable.Vector.newBuilder[netmsg.game.MInit.ExtractionSpeedRate] ++= this.extractionSpeedRates)
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __bounds = com.trueaccord.scalapb.LiteParser.readMessage(__input, __bounds)
          case 18 =>
            __objects += com.trueaccord.scalapb.LiteParser.readMessage(__input, netmsg.game.WObject.defaultInstance)
          case 26 =>
            __warpZone += com.trueaccord.scalapb.LiteParser.readMessage(__input, netmsg.base.Vect2.defaultInstance)
          case 34 =>
            __visiblePoints += com.trueaccord.scalapb.LiteParser.readMessage(__input, netmsg.base.Vect2.defaultInstance)
          case 42 =>
            __selfTeam = com.trueaccord.scalapb.LiteParser.readMessage(__input, __selfTeam)
          case 50 =>
            __otherTeams += com.trueaccord.scalapb.LiteParser.readMessage(__input, netmsg.game.Team.defaultInstance)
          case 58 =>
            __self = com.trueaccord.scalapb.LiteParser.readMessage(__input, __self)
          case 66 =>
            __otherPlayers += com.trueaccord.scalapb.LiteParser.readMessage(__input, netmsg.game.InitPlayer.defaultInstance)
          case 74 =>
            __warpableObjectStats = com.trueaccord.scalapb.LiteParser.readMessage(__input, __warpableObjectStats)
          case 82 =>
            __attackMultipliers += com.trueaccord.scalapb.LiteParser.readMessage(__input, netmsg.game.MInit.AttackMultiplier.defaultInstance)
          case 90 =>
            __objectives = com.trueaccord.scalapb.LiteParser.readMessage(__input, __objectives)
          case 98 =>
            __turnTimeframe = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __turnTimeframe.getOrElse(netmsg.base.Timeframe.defaultInstance)))
          case 106 =>
            __extractionSpeedRates += com.trueaccord.scalapb.LiteParser.readMessage(__input, netmsg.game.MInit.ExtractionSpeedRate.defaultInstance)
          case tag => __input.skipField(tag)
        }
      }
      netmsg.game.MInit(
          bounds = __bounds,
          objects = __objects.result(),
          warpZone = __warpZone.result(),
          visiblePoints = __visiblePoints.result(),
          selfTeam = __selfTeam,
          otherTeams = __otherTeams.result(),
          self = __self,
          otherPlayers = __otherPlayers.result(),
          warpableObjectStats = __warpableObjectStats,
          attackMultipliers = __attackMultipliers.result(),
          objectives = __objectives,
          turnTimeframe = __turnTimeframe,
          extractionSpeedRates = __extractionSpeedRates.result()
      )
    }
    def withBounds(__v: netmsg.base.Bounds): MInit = copy(bounds = __v)
    def clearObjects = copy(objects = Nil)
    def addObjects(__vs: netmsg.game.WObject*): MInit = addAllObjects(__vs)
    def addAllObjects(__vs: TraversableOnce[netmsg.game.WObject]): MInit = copy(objects = objects ++ __vs)
    def withObjects(__v: Seq[netmsg.game.WObject]): MInit = copy(objects = __v)
    def clearWarpZone = copy(warpZone = Nil)
    def addWarpZone(__vs: netmsg.base.Vect2*): MInit = addAllWarpZone(__vs)
    def addAllWarpZone(__vs: TraversableOnce[netmsg.base.Vect2]): MInit = copy(warpZone = warpZone ++ __vs)
    def withWarpZone(__v: Seq[netmsg.base.Vect2]): MInit = copy(warpZone = __v)
    def clearVisiblePoints = copy(visiblePoints = Nil)
    def addVisiblePoints(__vs: netmsg.base.Vect2*): MInit = addAllVisiblePoints(__vs)
    def addAllVisiblePoints(__vs: TraversableOnce[netmsg.base.Vect2]): MInit = copy(visiblePoints = visiblePoints ++ __vs)
    def withVisiblePoints(__v: Seq[netmsg.base.Vect2]): MInit = copy(visiblePoints = __v)
    def withSelfTeam(__v: netmsg.game.Team): MInit = copy(selfTeam = __v)
    def clearOtherTeams = copy(otherTeams = Nil)
    def addOtherTeams(__vs: netmsg.game.Team*): MInit = addAllOtherTeams(__vs)
    def addAllOtherTeams(__vs: TraversableOnce[netmsg.game.Team]): MInit = copy(otherTeams = otherTeams ++ __vs)
    def withOtherTeams(__v: Seq[netmsg.game.Team]): MInit = copy(otherTeams = __v)
    def withSelf(__v: netmsg.game.PlayerState): MInit = copy(self = __v)
    def clearOtherPlayers = copy(otherPlayers = Nil)
    def addOtherPlayers(__vs: netmsg.game.InitPlayer*): MInit = addAllOtherPlayers(__vs)
    def addAllOtherPlayers(__vs: TraversableOnce[netmsg.game.InitPlayer]): MInit = copy(otherPlayers = otherPlayers ++ __vs)
    def withOtherPlayers(__v: Seq[netmsg.game.InitPlayer]): MInit = copy(otherPlayers = __v)
    def withWarpableObjectStats(__v: netmsg.game.WarpableObjectStats): MInit = copy(warpableObjectStats = __v)
    def clearAttackMultipliers = copy(attackMultipliers = Nil)
    def addAttackMultipliers(__vs: netmsg.game.MInit.AttackMultiplier*): MInit = addAllAttackMultipliers(__vs)
    def addAllAttackMultipliers(__vs: TraversableOnce[netmsg.game.MInit.AttackMultiplier]): MInit = copy(attackMultipliers = attackMultipliers ++ __vs)
    def withAttackMultipliers(__v: Seq[netmsg.game.MInit.AttackMultiplier]): MInit = copy(attackMultipliers = __v)
    def withObjectives(__v: netmsg.game.Objectives): MInit = copy(objectives = __v)
    def getTurnTimeframe: netmsg.base.Timeframe = turnTimeframe.getOrElse(netmsg.base.Timeframe.defaultInstance)
    def clearTurnTimeframe: MInit = copy(turnTimeframe = None)
    def withTurnTimeframe(__v: netmsg.base.Timeframe): MInit = copy(turnTimeframe = Some(__v))
    def clearExtractionSpeedRates = copy(extractionSpeedRates = Nil)
    def addExtractionSpeedRates(__vs: netmsg.game.MInit.ExtractionSpeedRate*): MInit = addAllExtractionSpeedRates(__vs)
    def addAllExtractionSpeedRates(__vs: TraversableOnce[netmsg.game.MInit.ExtractionSpeedRate]): MInit = copy(extractionSpeedRates = extractionSpeedRates ++ __vs)
    def withExtractionSpeedRates(__v: Seq[netmsg.game.MInit.ExtractionSpeedRate]): MInit = copy(extractionSpeedRates = __v)
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => bounds
        case 2 => objects
        case 3 => warpZone
        case 4 => visiblePoints
        case 5 => selfTeam
        case 6 => otherTeams
        case 7 => self
        case 8 => otherPlayers
        case 9 => warpableObjectStats
        case 10 => attackMultipliers
        case 11 => objectives
        case 12 => turnTimeframe
        case 13 => extractionSpeedRates
      }
    }
    override def toString: String = com.google.protobuf.TextFormat.printToString(netmsg.game.MInit.toJavaProto(this))
    def companion = netmsg.game.MInit
}

object MInit extends com.trueaccord.scalapb.GeneratedMessageCompanion[MInit] with com.trueaccord.scalapb.JavaProtoSupport[MInit, netmsg.Game.MInit]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[MInit] with com.trueaccord.scalapb.JavaProtoSupport[MInit, netmsg.Game.MInit]  = this
  def toJavaProto(scalaPbSource: netmsg.game.MInit): netmsg.Game.MInit = {
    val javaPbOut = netmsg.Game.MInit.newBuilder
    javaPbOut.setBounds(netmsg.base.Bounds.toJavaProto(scalaPbSource.bounds))
    javaPbOut.addAllObjects(scalaPbSource.objects.map(netmsg.game.WObject.toJavaProto))
    javaPbOut.addAllWarpZone(scalaPbSource.warpZone.map(netmsg.base.Vect2.toJavaProto))
    javaPbOut.addAllVisiblePoints(scalaPbSource.visiblePoints.map(netmsg.base.Vect2.toJavaProto))
    javaPbOut.setSelfTeam(netmsg.game.Team.toJavaProto(scalaPbSource.selfTeam))
    javaPbOut.addAllOtherTeams(scalaPbSource.otherTeams.map(netmsg.game.Team.toJavaProto))
    javaPbOut.setSelf(netmsg.game.PlayerState.toJavaProto(scalaPbSource.self))
    javaPbOut.addAllOtherPlayers(scalaPbSource.otherPlayers.map(netmsg.game.InitPlayer.toJavaProto))
    javaPbOut.setWarpableObjectStats(netmsg.game.WarpableObjectStats.toJavaProto(scalaPbSource.warpableObjectStats))
    javaPbOut.addAllAttackMultipliers(scalaPbSource.attackMultipliers.map(netmsg.game.MInit.AttackMultiplier.toJavaProto))
    javaPbOut.setObjectives(netmsg.game.Objectives.toJavaProto(scalaPbSource.objectives))
    scalaPbSource.turnTimeframe.map(netmsg.base.Timeframe.toJavaProto).foreach(javaPbOut.setTurnTimeframe)
    javaPbOut.addAllExtractionSpeedRates(scalaPbSource.extractionSpeedRates.map(netmsg.game.MInit.ExtractionSpeedRate.toJavaProto))
    javaPbOut.build
  }
  def fromJavaProto(javaPbSource: netmsg.Game.MInit): netmsg.game.MInit = netmsg.game.MInit(
    bounds = netmsg.base.Bounds.fromJavaProto(javaPbSource.getBounds),
    objects = javaPbSource.getObjectsList.map(netmsg.game.WObject.fromJavaProto),
    warpZone = javaPbSource.getWarpZoneList.map(netmsg.base.Vect2.fromJavaProto),
    visiblePoints = javaPbSource.getVisiblePointsList.map(netmsg.base.Vect2.fromJavaProto),
    selfTeam = netmsg.game.Team.fromJavaProto(javaPbSource.getSelfTeam),
    otherTeams = javaPbSource.getOtherTeamsList.map(netmsg.game.Team.fromJavaProto),
    self = netmsg.game.PlayerState.fromJavaProto(javaPbSource.getSelf),
    otherPlayers = javaPbSource.getOtherPlayersList.map(netmsg.game.InitPlayer.fromJavaProto),
    warpableObjectStats = netmsg.game.WarpableObjectStats.fromJavaProto(javaPbSource.getWarpableObjectStats),
    attackMultipliers = javaPbSource.getAttackMultipliersList.map(netmsg.game.MInit.AttackMultiplier.fromJavaProto),
    objectives = netmsg.game.Objectives.fromJavaProto(javaPbSource.getObjectives),
    turnTimeframe = if (javaPbSource.hasTurnTimeframe) Some(netmsg.base.Timeframe.fromJavaProto(javaPbSource.getTurnTimeframe)) else None,
    extractionSpeedRates = javaPbSource.getExtractionSpeedRatesList.map(netmsg.game.MInit.ExtractionSpeedRate.fromJavaProto)
  )
  override def fromAscii(ascii: String): netmsg.game.MInit = {
    val javaProtoBuilder = netmsg.Game.MInit.newBuilder
    com.google.protobuf.TextFormat.merge(ascii, javaProtoBuilder)
    fromJavaProto(javaProtoBuilder.build)
  }
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.game.MInit = netmsg.game.MInit(
    bounds = fieldsMap(1).asInstanceOf[netmsg.base.Bounds],
    objects = fieldsMap.getOrElse(2, Nil).asInstanceOf[Seq[netmsg.game.WObject]],
    warpZone = fieldsMap.getOrElse(3, Nil).asInstanceOf[Seq[netmsg.base.Vect2]],
    visiblePoints = fieldsMap.getOrElse(4, Nil).asInstanceOf[Seq[netmsg.base.Vect2]],
    selfTeam = fieldsMap(5).asInstanceOf[netmsg.game.Team],
    otherTeams = fieldsMap.getOrElse(6, Nil).asInstanceOf[Seq[netmsg.game.Team]],
    self = fieldsMap(7).asInstanceOf[netmsg.game.PlayerState],
    otherPlayers = fieldsMap.getOrElse(8, Nil).asInstanceOf[Seq[netmsg.game.InitPlayer]],
    warpableObjectStats = fieldsMap(9).asInstanceOf[netmsg.game.WarpableObjectStats],
    attackMultipliers = fieldsMap.getOrElse(10, Nil).asInstanceOf[Seq[netmsg.game.MInit.AttackMultiplier]],
    objectives = fieldsMap(11).asInstanceOf[netmsg.game.Objectives],
    turnTimeframe = fieldsMap.getOrElse(12, None).asInstanceOf[Option[netmsg.base.Timeframe]],
    extractionSpeedRates = fieldsMap.getOrElse(13, Nil).asInstanceOf[Seq[netmsg.game.MInit.ExtractionSpeedRate]]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("MInit", this,
    None, m = Seq(netmsg.game.MInit.AttackMultiplier.descriptor, netmsg.game.MInit.ExtractionSpeedRate.descriptor),
    e = Seq(),
    f = netmsg.game.InternalFields_game.internalFieldsFor("netmsg.game.MInit"))
  lazy val defaultInstance = netmsg.game.MInit(
    bounds = netmsg.base.Bounds.defaultInstance,
    selfTeam = netmsg.game.Team.defaultInstance,
    self = netmsg.game.PlayerState.defaultInstance,
    warpableObjectStats = netmsg.game.WarpableObjectStats.defaultInstance,
    objectives = netmsg.game.Objectives.defaultInstance
  )
  final case class AttackMultiplier(
      fromKind: netmsg.game.WObjKind,
      toKind: netmsg.game.WObjKind,
      multiplier: Float
      ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[AttackMultiplier] with com.trueaccord.lenses.Updatable[AttackMultiplier] {
      lazy val serializedSize: Int = {
        var __size = 0
        __size += com.google.protobuf.CodedOutputStream.computeEnumSize(1, fromKind.id)
        __size += com.google.protobuf.CodedOutputStream.computeEnumSize(2, toKind.id)
        __size += com.google.protobuf.CodedOutputStream.computeFloatSize(3, multiplier)
        __size
      }
      def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
        output.writeEnum(1, fromKind.id)
        output.writeEnum(2, toKind.id)
        output.writeFloat(3, multiplier)
      }
      def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.game.MInit.AttackMultiplier = {
        var __fromKind = this.fromKind
        var __toKind = this.toKind
        var __multiplier = this.multiplier
        var _done__ = false
        while (!_done__) {
          val _tag__ = __input.readTag()
          _tag__ match {
            case 0 => _done__ = true
            case 8 =>
              __fromKind = netmsg.game.WObjKind.fromValue(__input.readEnum())
            case 16 =>
              __toKind = netmsg.game.WObjKind.fromValue(__input.readEnum())
            case 29 =>
              __multiplier = __input.readFloat()
            case tag => __input.skipField(tag)
          }
        }
        netmsg.game.MInit.AttackMultiplier(
            fromKind = __fromKind,
            toKind = __toKind,
            multiplier = __multiplier
        )
      }
      def withFromKind(__v: netmsg.game.WObjKind): AttackMultiplier = copy(fromKind = __v)
      def withToKind(__v: netmsg.game.WObjKind): AttackMultiplier = copy(toKind = __v)
      def withMultiplier(__v: Float): AttackMultiplier = copy(multiplier = __v)
      def getField(__field: Descriptors.FieldDescriptor): Any = {
        __field.number match {
          case 1 => fromKind
          case 2 => toKind
          case 3 => multiplier
        }
      }
      override def toString: String = com.google.protobuf.TextFormat.printToString(netmsg.game.MInit.AttackMultiplier.toJavaProto(this))
      def companion = netmsg.game.MInit.AttackMultiplier
  }
  
  object AttackMultiplier extends com.trueaccord.scalapb.GeneratedMessageCompanion[AttackMultiplier] with com.trueaccord.scalapb.JavaProtoSupport[AttackMultiplier, netmsg.Game.MInit.AttackMultiplier]  {
    implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[AttackMultiplier] with com.trueaccord.scalapb.JavaProtoSupport[AttackMultiplier, netmsg.Game.MInit.AttackMultiplier]  = this
    def toJavaProto(scalaPbSource: netmsg.game.MInit.AttackMultiplier): netmsg.Game.MInit.AttackMultiplier = {
      val javaPbOut = netmsg.Game.MInit.AttackMultiplier.newBuilder
      javaPbOut.setFromKind(netmsg.game.WObjKind.toJavaValue(scalaPbSource.fromKind))
      javaPbOut.setToKind(netmsg.game.WObjKind.toJavaValue(scalaPbSource.toKind))
      javaPbOut.setMultiplier(scalaPbSource.multiplier)
      javaPbOut.build
    }
    def fromJavaProto(javaPbSource: netmsg.Game.MInit.AttackMultiplier): netmsg.game.MInit.AttackMultiplier = netmsg.game.MInit.AttackMultiplier(
      fromKind = netmsg.game.WObjKind.fromJavaValue(javaPbSource.getFromKind),
      toKind = netmsg.game.WObjKind.fromJavaValue(javaPbSource.getToKind),
      multiplier = javaPbSource.getMultiplier.floatValue
    )
    override def fromAscii(ascii: String): netmsg.game.MInit.AttackMultiplier = {
      val javaProtoBuilder = netmsg.Game.MInit.AttackMultiplier.newBuilder
      com.google.protobuf.TextFormat.merge(ascii, javaProtoBuilder)
      fromJavaProto(javaProtoBuilder.build)
    }
    def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.game.MInit.AttackMultiplier = netmsg.game.MInit.AttackMultiplier(
      fromKind = fieldsMap(1).asInstanceOf[netmsg.game.WObjKind],
      toKind = fieldsMap(2).asInstanceOf[netmsg.game.WObjKind],
      multiplier = fieldsMap(3).asInstanceOf[Float]
    )
    lazy val descriptor = new Descriptors.MessageDescriptor("AttackMultiplier", this,
      None, m = Seq(),
      e = Seq(),
      f = netmsg.game.InternalFields_game.internalFieldsFor("netmsg.game.MInit.AttackMultiplier"))
    lazy val defaultInstance = netmsg.game.MInit.AttackMultiplier(
      fromKind = netmsg.game.WObjKind.LIGHT,
      toKind = netmsg.game.WObjKind.LIGHT,
      multiplier = 0.0f
    )
    implicit class AttackMultiplierLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, AttackMultiplier]) extends com.trueaccord.lenses.ObjectLens[UpperPB, AttackMultiplier](_l) {
      def fromKind: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObjKind] = field(_.fromKind)((c_, f_) => c_.copy(fromKind = f_))
      def toKind: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObjKind] = field(_.toKind)((c_, f_) => c_.copy(toKind = f_))
      def multiplier: com.trueaccord.lenses.Lens[UpperPB, Float] = field(_.multiplier)((c_, f_) => c_.copy(multiplier = f_))
    }
    final val FROM_KIND_FIELD_NUMBER = 1
    final val TO_KIND_FIELD_NUMBER = 2
    final val MULTIPLIER_FIELD_NUMBER = 3
  }
  
  final case class ExtractionSpeedRate(
      extractionSpeed: netmsg.game.WObject.Asteroid.ExtractionSpeed,
      resourcesPerTurn: Int
      ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[ExtractionSpeedRate] with com.trueaccord.lenses.Updatable[ExtractionSpeedRate] {
      lazy val serializedSize: Int = {
        var __size = 0
        __size += com.google.protobuf.CodedOutputStream.computeEnumSize(1, extractionSpeed.id)
        __size += com.google.protobuf.CodedOutputStream.computeUInt32Size(2, resourcesPerTurn)
        __size
      }
      def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
        output.writeEnum(1, extractionSpeed.id)
        output.writeUInt32(2, resourcesPerTurn)
      }
      def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.game.MInit.ExtractionSpeedRate = {
        var __extractionSpeed = this.extractionSpeed
        var __resourcesPerTurn = this.resourcesPerTurn
        var _done__ = false
        while (!_done__) {
          val _tag__ = __input.readTag()
          _tag__ match {
            case 0 => _done__ = true
            case 8 =>
              __extractionSpeed = netmsg.game.WObject.Asteroid.ExtractionSpeed.fromValue(__input.readEnum())
            case 16 =>
              __resourcesPerTurn = __input.readUInt32()
            case tag => __input.skipField(tag)
          }
        }
        netmsg.game.MInit.ExtractionSpeedRate(
            extractionSpeed = __extractionSpeed,
            resourcesPerTurn = __resourcesPerTurn
        )
      }
      def withExtractionSpeed(__v: netmsg.game.WObject.Asteroid.ExtractionSpeed): ExtractionSpeedRate = copy(extractionSpeed = __v)
      def withResourcesPerTurn(__v: Int): ExtractionSpeedRate = copy(resourcesPerTurn = __v)
      def getField(__field: Descriptors.FieldDescriptor): Any = {
        __field.number match {
          case 1 => extractionSpeed
          case 2 => resourcesPerTurn
        }
      }
      override def toString: String = com.google.protobuf.TextFormat.printToString(netmsg.game.MInit.ExtractionSpeedRate.toJavaProto(this))
      def companion = netmsg.game.MInit.ExtractionSpeedRate
  }
  
  object ExtractionSpeedRate extends com.trueaccord.scalapb.GeneratedMessageCompanion[ExtractionSpeedRate] with com.trueaccord.scalapb.JavaProtoSupport[ExtractionSpeedRate, netmsg.Game.MInit.ExtractionSpeedRate]  {
    implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[ExtractionSpeedRate] with com.trueaccord.scalapb.JavaProtoSupport[ExtractionSpeedRate, netmsg.Game.MInit.ExtractionSpeedRate]  = this
    def toJavaProto(scalaPbSource: netmsg.game.MInit.ExtractionSpeedRate): netmsg.Game.MInit.ExtractionSpeedRate = {
      val javaPbOut = netmsg.Game.MInit.ExtractionSpeedRate.newBuilder
      javaPbOut.setExtractionSpeed(netmsg.game.WObject.Asteroid.ExtractionSpeed.toJavaValue(scalaPbSource.extractionSpeed))
      javaPbOut.setResourcesPerTurn(scalaPbSource.resourcesPerTurn)
      javaPbOut.build
    }
    def fromJavaProto(javaPbSource: netmsg.Game.MInit.ExtractionSpeedRate): netmsg.game.MInit.ExtractionSpeedRate = netmsg.game.MInit.ExtractionSpeedRate(
      extractionSpeed = netmsg.game.WObject.Asteroid.ExtractionSpeed.fromJavaValue(javaPbSource.getExtractionSpeed),
      resourcesPerTurn = javaPbSource.getResourcesPerTurn.intValue
    )
    override def fromAscii(ascii: String): netmsg.game.MInit.ExtractionSpeedRate = {
      val javaProtoBuilder = netmsg.Game.MInit.ExtractionSpeedRate.newBuilder
      com.google.protobuf.TextFormat.merge(ascii, javaProtoBuilder)
      fromJavaProto(javaProtoBuilder.build)
    }
    def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.game.MInit.ExtractionSpeedRate = netmsg.game.MInit.ExtractionSpeedRate(
      extractionSpeed = fieldsMap(1).asInstanceOf[netmsg.game.WObject.Asteroid.ExtractionSpeed],
      resourcesPerTurn = fieldsMap(2).asInstanceOf[Int]
    )
    lazy val descriptor = new Descriptors.MessageDescriptor("ExtractionSpeedRate", this,
      None, m = Seq(),
      e = Seq(),
      f = netmsg.game.InternalFields_game.internalFieldsFor("netmsg.game.MInit.ExtractionSpeedRate"))
    lazy val defaultInstance = netmsg.game.MInit.ExtractionSpeedRate(
      extractionSpeed = netmsg.game.WObject.Asteroid.ExtractionSpeed.SLOW,
      resourcesPerTurn = 0
    )
    implicit class ExtractionSpeedRateLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, ExtractionSpeedRate]) extends com.trueaccord.lenses.ObjectLens[UpperPB, ExtractionSpeedRate](_l) {
      def extractionSpeed: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObject.Asteroid.ExtractionSpeed] = field(_.extractionSpeed)((c_, f_) => c_.copy(extractionSpeed = f_))
      def resourcesPerTurn: com.trueaccord.lenses.Lens[UpperPB, Int] = field(_.resourcesPerTurn)((c_, f_) => c_.copy(resourcesPerTurn = f_))
    }
    final val EXTRACTION_SPEED_FIELD_NUMBER = 1
    final val RESOURCES_PER_TURN_FIELD_NUMBER = 2
  }
  
  implicit class MInitLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, MInit]) extends com.trueaccord.lenses.ObjectLens[UpperPB, MInit](_l) {
    def bounds: com.trueaccord.lenses.Lens[UpperPB, netmsg.base.Bounds] = field(_.bounds)((c_, f_) => c_.copy(bounds = f_))
    def objects: com.trueaccord.lenses.Lens[UpperPB, Seq[netmsg.game.WObject]] = field(_.objects)((c_, f_) => c_.copy(objects = f_))
    def warpZone: com.trueaccord.lenses.Lens[UpperPB, Seq[netmsg.base.Vect2]] = field(_.warpZone)((c_, f_) => c_.copy(warpZone = f_))
    def visiblePoints: com.trueaccord.lenses.Lens[UpperPB, Seq[netmsg.base.Vect2]] = field(_.visiblePoints)((c_, f_) => c_.copy(visiblePoints = f_))
    def selfTeam: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.Team] = field(_.selfTeam)((c_, f_) => c_.copy(selfTeam = f_))
    def otherTeams: com.trueaccord.lenses.Lens[UpperPB, Seq[netmsg.game.Team]] = field(_.otherTeams)((c_, f_) => c_.copy(otherTeams = f_))
    def self: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.PlayerState] = field(_.self)((c_, f_) => c_.copy(self = f_))
    def otherPlayers: com.trueaccord.lenses.Lens[UpperPB, Seq[netmsg.game.InitPlayer]] = field(_.otherPlayers)((c_, f_) => c_.copy(otherPlayers = f_))
    def warpableObjectStats: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WarpableObjectStats] = field(_.warpableObjectStats)((c_, f_) => c_.copy(warpableObjectStats = f_))
    def attackMultipliers: com.trueaccord.lenses.Lens[UpperPB, Seq[netmsg.game.MInit.AttackMultiplier]] = field(_.attackMultipliers)((c_, f_) => c_.copy(attackMultipliers = f_))
    def objectives: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.Objectives] = field(_.objectives)((c_, f_) => c_.copy(objectives = f_))
    def turnTimeframe: com.trueaccord.lenses.Lens[UpperPB, netmsg.base.Timeframe] = field(_.getTurnTimeframe)((c_, f_) => c_.copy(turnTimeframe = Some(f_)))
    def optionalTurnTimeframe: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.base.Timeframe]] = field(_.turnTimeframe)((c_, f_) => c_.copy(turnTimeframe = f_))
    def extractionSpeedRates: com.trueaccord.lenses.Lens[UpperPB, Seq[netmsg.game.MInit.ExtractionSpeedRate]] = field(_.extractionSpeedRates)((c_, f_) => c_.copy(extractionSpeedRates = f_))
  }
  final val BOUNDS_FIELD_NUMBER = 1
  final val OBJECTS_FIELD_NUMBER = 2
  final val WARP_ZONE_FIELD_NUMBER = 3
  final val VISIBLE_POINTS_FIELD_NUMBER = 4
  final val SELF_TEAM_FIELD_NUMBER = 5
  final val OTHER_TEAMS_FIELD_NUMBER = 6
  final val SELF_FIELD_NUMBER = 7
  final val OTHER_PLAYERS_FIELD_NUMBER = 8
  final val WARPABLE_OBJECT_STATS_FIELD_NUMBER = 9
  final val ATTACK_MULTIPLIERS_FIELD_NUMBER = 10
  final val OBJECTIVES_FIELD_NUMBER = 11
  final val TURN_TIMEFRAME_FIELD_NUMBER = 12
  final val EXTRACTION_SPEED_RATES_FIELD_NUMBER = 13
}
