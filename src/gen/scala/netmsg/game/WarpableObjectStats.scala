// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.game


import com.trueaccord.scalapb.Descriptors

final case class WarpableObjectStats(
    extractor: Option[netmsg.game.WObjectStats.Extractor] = None,
    warpLinker: Option[netmsg.game.WObjectStats.WarpLinker] = None,
    populationTower: Option[netmsg.game.WObjectStats.PopulationTower] = None,
    laserTower: Option[netmsg.game.WObjectStats.LaserTower] = None,
    corvette: Option[netmsg.game.WObjectStats.Corvette] = None,
    wasp: Option[netmsg.game.WObjectStats.Wasp] = None,
    scout: Option[netmsg.game.WObjectStats.Scout] = None,
    rayShip: Option[netmsg.game.WObjectStats.RayShip] = None,
    rocketFrigate: Option[netmsg.game.WObjectStats.RocketFrigate] = None,
    gunship: Option[netmsg.game.WObjectStats.Gunship] = None,
    fortress: Option[netmsg.game.WObjectStats.Fortress] = None,
    drone: Option[netmsg.game.WObjectStats.Drone] = None
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[WarpableObjectStats] with com.trueaccord.lenses.Updatable[WarpableObjectStats] {
    lazy val serializedSize: Int = {
      var __size = 0
      if (extractor.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(extractor.get.serializedSize) + extractor.get.serializedSize }
      if (warpLinker.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(warpLinker.get.serializedSize) + warpLinker.get.serializedSize }
      if (populationTower.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(populationTower.get.serializedSize) + populationTower.get.serializedSize }
      if (laserTower.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(laserTower.get.serializedSize) + laserTower.get.serializedSize }
      if (corvette.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(corvette.get.serializedSize) + corvette.get.serializedSize }
      if (wasp.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(wasp.get.serializedSize) + wasp.get.serializedSize }
      if (scout.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(scout.get.serializedSize) + scout.get.serializedSize }
      if (rayShip.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(rayShip.get.serializedSize) + rayShip.get.serializedSize }
      if (rocketFrigate.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(rocketFrigate.get.serializedSize) + rocketFrigate.get.serializedSize }
      if (gunship.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(gunship.get.serializedSize) + gunship.get.serializedSize }
      if (fortress.isDefined) { __size += 2 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(fortress.get.serializedSize) + fortress.get.serializedSize }
      if (drone.isDefined) { __size += 2 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(drone.get.serializedSize) + drone.get.serializedSize }
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      extractor.foreach { v => 
        output.writeTag(6, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      warpLinker.foreach { v => 
        output.writeTag(7, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      populationTower.foreach { v => 
        output.writeTag(8, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      laserTower.foreach { v => 
        output.writeTag(9, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      corvette.foreach { v => 
        output.writeTag(10, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      wasp.foreach { v => 
        output.writeTag(11, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      scout.foreach { v => 
        output.writeTag(12, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      rayShip.foreach { v => 
        output.writeTag(13, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      rocketFrigate.foreach { v => 
        output.writeTag(14, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      gunship.foreach { v => 
        output.writeTag(15, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      fortress.foreach { v => 
        output.writeTag(16, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      drone.foreach { v => 
        output.writeTag(17, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.game.WarpableObjectStats = {
      var __extractor = this.extractor
      var __warpLinker = this.warpLinker
      var __populationTower = this.populationTower
      var __laserTower = this.laserTower
      var __corvette = this.corvette
      var __wasp = this.wasp
      var __scout = this.scout
      var __rayShip = this.rayShip
      var __rocketFrigate = this.rocketFrigate
      var __gunship = this.gunship
      var __fortress = this.fortress
      var __drone = this.drone
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 50 =>
            __extractor = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __extractor.getOrElse(netmsg.game.WObjectStats.Extractor.defaultInstance)))
          case 58 =>
            __warpLinker = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __warpLinker.getOrElse(netmsg.game.WObjectStats.WarpLinker.defaultInstance)))
          case 66 =>
            __populationTower = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __populationTower.getOrElse(netmsg.game.WObjectStats.PopulationTower.defaultInstance)))
          case 74 =>
            __laserTower = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __laserTower.getOrElse(netmsg.game.WObjectStats.LaserTower.defaultInstance)))
          case 82 =>
            __corvette = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __corvette.getOrElse(netmsg.game.WObjectStats.Corvette.defaultInstance)))
          case 90 =>
            __wasp = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __wasp.getOrElse(netmsg.game.WObjectStats.Wasp.defaultInstance)))
          case 98 =>
            __scout = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __scout.getOrElse(netmsg.game.WObjectStats.Scout.defaultInstance)))
          case 106 =>
            __rayShip = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __rayShip.getOrElse(netmsg.game.WObjectStats.RayShip.defaultInstance)))
          case 114 =>
            __rocketFrigate = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __rocketFrigate.getOrElse(netmsg.game.WObjectStats.RocketFrigate.defaultInstance)))
          case 122 =>
            __gunship = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __gunship.getOrElse(netmsg.game.WObjectStats.Gunship.defaultInstance)))
          case 130 =>
            __fortress = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __fortress.getOrElse(netmsg.game.WObjectStats.Fortress.defaultInstance)))
          case 138 =>
            __drone = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __drone.getOrElse(netmsg.game.WObjectStats.Drone.defaultInstance)))
          case tag => __input.skipField(tag)
        }
      }
      netmsg.game.WarpableObjectStats(
          extractor = __extractor,
          warpLinker = __warpLinker,
          populationTower = __populationTower,
          laserTower = __laserTower,
          corvette = __corvette,
          wasp = __wasp,
          scout = __scout,
          rayShip = __rayShip,
          rocketFrigate = __rocketFrigate,
          gunship = __gunship,
          fortress = __fortress,
          drone = __drone
      )
    }
    def getExtractor: netmsg.game.WObjectStats.Extractor = extractor.getOrElse(netmsg.game.WObjectStats.Extractor.defaultInstance)
    def clearExtractor: WarpableObjectStats = copy(extractor = None)
    def withExtractor(__v: netmsg.game.WObjectStats.Extractor): WarpableObjectStats = copy(extractor = Some(__v))
    def getWarpLinker: netmsg.game.WObjectStats.WarpLinker = warpLinker.getOrElse(netmsg.game.WObjectStats.WarpLinker.defaultInstance)
    def clearWarpLinker: WarpableObjectStats = copy(warpLinker = None)
    def withWarpLinker(__v: netmsg.game.WObjectStats.WarpLinker): WarpableObjectStats = copy(warpLinker = Some(__v))
    def getPopulationTower: netmsg.game.WObjectStats.PopulationTower = populationTower.getOrElse(netmsg.game.WObjectStats.PopulationTower.defaultInstance)
    def clearPopulationTower: WarpableObjectStats = copy(populationTower = None)
    def withPopulationTower(__v: netmsg.game.WObjectStats.PopulationTower): WarpableObjectStats = copy(populationTower = Some(__v))
    def getLaserTower: netmsg.game.WObjectStats.LaserTower = laserTower.getOrElse(netmsg.game.WObjectStats.LaserTower.defaultInstance)
    def clearLaserTower: WarpableObjectStats = copy(laserTower = None)
    def withLaserTower(__v: netmsg.game.WObjectStats.LaserTower): WarpableObjectStats = copy(laserTower = Some(__v))
    def getCorvette: netmsg.game.WObjectStats.Corvette = corvette.getOrElse(netmsg.game.WObjectStats.Corvette.defaultInstance)
    def clearCorvette: WarpableObjectStats = copy(corvette = None)
    def withCorvette(__v: netmsg.game.WObjectStats.Corvette): WarpableObjectStats = copy(corvette = Some(__v))
    def getWasp: netmsg.game.WObjectStats.Wasp = wasp.getOrElse(netmsg.game.WObjectStats.Wasp.defaultInstance)
    def clearWasp: WarpableObjectStats = copy(wasp = None)
    def withWasp(__v: netmsg.game.WObjectStats.Wasp): WarpableObjectStats = copy(wasp = Some(__v))
    def getScout: netmsg.game.WObjectStats.Scout = scout.getOrElse(netmsg.game.WObjectStats.Scout.defaultInstance)
    def clearScout: WarpableObjectStats = copy(scout = None)
    def withScout(__v: netmsg.game.WObjectStats.Scout): WarpableObjectStats = copy(scout = Some(__v))
    def getRayShip: netmsg.game.WObjectStats.RayShip = rayShip.getOrElse(netmsg.game.WObjectStats.RayShip.defaultInstance)
    def clearRayShip: WarpableObjectStats = copy(rayShip = None)
    def withRayShip(__v: netmsg.game.WObjectStats.RayShip): WarpableObjectStats = copy(rayShip = Some(__v))
    def getRocketFrigate: netmsg.game.WObjectStats.RocketFrigate = rocketFrigate.getOrElse(netmsg.game.WObjectStats.RocketFrigate.defaultInstance)
    def clearRocketFrigate: WarpableObjectStats = copy(rocketFrigate = None)
    def withRocketFrigate(__v: netmsg.game.WObjectStats.RocketFrigate): WarpableObjectStats = copy(rocketFrigate = Some(__v))
    def getGunship: netmsg.game.WObjectStats.Gunship = gunship.getOrElse(netmsg.game.WObjectStats.Gunship.defaultInstance)
    def clearGunship: WarpableObjectStats = copy(gunship = None)
    def withGunship(__v: netmsg.game.WObjectStats.Gunship): WarpableObjectStats = copy(gunship = Some(__v))
    def getFortress: netmsg.game.WObjectStats.Fortress = fortress.getOrElse(netmsg.game.WObjectStats.Fortress.defaultInstance)
    def clearFortress: WarpableObjectStats = copy(fortress = None)
    def withFortress(__v: netmsg.game.WObjectStats.Fortress): WarpableObjectStats = copy(fortress = Some(__v))
    def getDrone: netmsg.game.WObjectStats.Drone = drone.getOrElse(netmsg.game.WObjectStats.Drone.defaultInstance)
    def clearDrone: WarpableObjectStats = copy(drone = None)
    def withDrone(__v: netmsg.game.WObjectStats.Drone): WarpableObjectStats = copy(drone = Some(__v))
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 6 => extractor
        case 7 => warpLinker
        case 8 => populationTower
        case 9 => laserTower
        case 10 => corvette
        case 11 => wasp
        case 12 => scout
        case 13 => rayShip
        case 14 => rocketFrigate
        case 15 => gunship
        case 16 => fortress
        case 17 => drone
      }
    }
    def companion = netmsg.game.WarpableObjectStats
}

object WarpableObjectStats extends com.trueaccord.scalapb.GeneratedMessageCompanion[WarpableObjectStats]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[WarpableObjectStats]  = this
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.game.WarpableObjectStats = netmsg.game.WarpableObjectStats(
    extractor = fieldsMap.getOrElse(6, None).asInstanceOf[Option[netmsg.game.WObjectStats.Extractor]],
    warpLinker = fieldsMap.getOrElse(7, None).asInstanceOf[Option[netmsg.game.WObjectStats.WarpLinker]],
    populationTower = fieldsMap.getOrElse(8, None).asInstanceOf[Option[netmsg.game.WObjectStats.PopulationTower]],
    laserTower = fieldsMap.getOrElse(9, None).asInstanceOf[Option[netmsg.game.WObjectStats.LaserTower]],
    corvette = fieldsMap.getOrElse(10, None).asInstanceOf[Option[netmsg.game.WObjectStats.Corvette]],
    wasp = fieldsMap.getOrElse(11, None).asInstanceOf[Option[netmsg.game.WObjectStats.Wasp]],
    scout = fieldsMap.getOrElse(12, None).asInstanceOf[Option[netmsg.game.WObjectStats.Scout]],
    rayShip = fieldsMap.getOrElse(13, None).asInstanceOf[Option[netmsg.game.WObjectStats.RayShip]],
    rocketFrigate = fieldsMap.getOrElse(14, None).asInstanceOf[Option[netmsg.game.WObjectStats.RocketFrigate]],
    gunship = fieldsMap.getOrElse(15, None).asInstanceOf[Option[netmsg.game.WObjectStats.Gunship]],
    fortress = fieldsMap.getOrElse(16, None).asInstanceOf[Option[netmsg.game.WObjectStats.Fortress]],
    drone = fieldsMap.getOrElse(17, None).asInstanceOf[Option[netmsg.game.WObjectStats.Drone]]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("WarpableObjectStats", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.game.InternalFields_game.internalFieldsFor("netmsg.game.WarpableObjectStats"))
  lazy val defaultInstance = netmsg.game.WarpableObjectStats(
  )
  implicit class WarpableObjectStatsLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, WarpableObjectStats]) extends com.trueaccord.lenses.ObjectLens[UpperPB, WarpableObjectStats](_l) {
    def extractor: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObjectStats.Extractor] = field(_.getExtractor)((c_, f_) => c_.copy(extractor = Some(f_)))
    def optionalExtractor: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.WObjectStats.Extractor]] = field(_.extractor)((c_, f_) => c_.copy(extractor = f_))
    def warpLinker: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObjectStats.WarpLinker] = field(_.getWarpLinker)((c_, f_) => c_.copy(warpLinker = Some(f_)))
    def optionalWarpLinker: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.WObjectStats.WarpLinker]] = field(_.warpLinker)((c_, f_) => c_.copy(warpLinker = f_))
    def populationTower: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObjectStats.PopulationTower] = field(_.getPopulationTower)((c_, f_) => c_.copy(populationTower = Some(f_)))
    def optionalPopulationTower: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.WObjectStats.PopulationTower]] = field(_.populationTower)((c_, f_) => c_.copy(populationTower = f_))
    def laserTower: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObjectStats.LaserTower] = field(_.getLaserTower)((c_, f_) => c_.copy(laserTower = Some(f_)))
    def optionalLaserTower: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.WObjectStats.LaserTower]] = field(_.laserTower)((c_, f_) => c_.copy(laserTower = f_))
    def corvette: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObjectStats.Corvette] = field(_.getCorvette)((c_, f_) => c_.copy(corvette = Some(f_)))
    def optionalCorvette: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.WObjectStats.Corvette]] = field(_.corvette)((c_, f_) => c_.copy(corvette = f_))
    def wasp: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObjectStats.Wasp] = field(_.getWasp)((c_, f_) => c_.copy(wasp = Some(f_)))
    def optionalWasp: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.WObjectStats.Wasp]] = field(_.wasp)((c_, f_) => c_.copy(wasp = f_))
    def scout: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObjectStats.Scout] = field(_.getScout)((c_, f_) => c_.copy(scout = Some(f_)))
    def optionalScout: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.WObjectStats.Scout]] = field(_.scout)((c_, f_) => c_.copy(scout = f_))
    def rayShip: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObjectStats.RayShip] = field(_.getRayShip)((c_, f_) => c_.copy(rayShip = Some(f_)))
    def optionalRayShip: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.WObjectStats.RayShip]] = field(_.rayShip)((c_, f_) => c_.copy(rayShip = f_))
    def rocketFrigate: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObjectStats.RocketFrigate] = field(_.getRocketFrigate)((c_, f_) => c_.copy(rocketFrigate = Some(f_)))
    def optionalRocketFrigate: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.WObjectStats.RocketFrigate]] = field(_.rocketFrigate)((c_, f_) => c_.copy(rocketFrigate = f_))
    def gunship: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObjectStats.Gunship] = field(_.getGunship)((c_, f_) => c_.copy(gunship = Some(f_)))
    def optionalGunship: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.WObjectStats.Gunship]] = field(_.gunship)((c_, f_) => c_.copy(gunship = f_))
    def fortress: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObjectStats.Fortress] = field(_.getFortress)((c_, f_) => c_.copy(fortress = Some(f_)))
    def optionalFortress: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.WObjectStats.Fortress]] = field(_.fortress)((c_, f_) => c_.copy(fortress = f_))
    def drone: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WObjectStats.Drone] = field(_.getDrone)((c_, f_) => c_.copy(drone = Some(f_)))
    def optionalDrone: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.WObjectStats.Drone]] = field(_.drone)((c_, f_) => c_.copy(drone = f_))
  }
  final val EXTRACTOR_FIELD_NUMBER = 6
  final val WARP_LINKER_FIELD_NUMBER = 7
  final val POPULATION_TOWER_FIELD_NUMBER = 8
  final val LASER_TOWER_FIELD_NUMBER = 9
  final val CORVETTE_FIELD_NUMBER = 10
  final val WASP_FIELD_NUMBER = 11
  final val SCOUT_FIELD_NUMBER = 12
  final val RAY_SHIP_FIELD_NUMBER = 13
  final val ROCKET_FRIGATE_FIELD_NUMBER = 14
  final val GUNSHIP_FIELD_NUMBER = 15
  final val FORTRESS_FIELD_NUMBER = 16
  final val DRONE_FIELD_NUMBER = 17
}
