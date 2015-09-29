// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.game


import com.trueaccord.scalapb.Descriptors

final case class PlayerState(
    resources: Int,
    actions: Int,
    turnEnded: Boolean,
    population: netmsg.base.ValWithMax
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[PlayerState] with com.trueaccord.lenses.Updatable[PlayerState] {
    lazy val serializedSize: Int = {
      var __size = 0
      __size += com.google.protobuf.CodedOutputStream.computeUInt32Size(1, resources)
      __size += com.google.protobuf.CodedOutputStream.computeUInt32Size(2, actions)
      __size += com.google.protobuf.CodedOutputStream.computeBoolSize(3, turnEnded)
      __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(population.serializedSize) + population.serializedSize
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      output.writeUInt32(1, resources)
      output.writeUInt32(2, actions)
      output.writeBool(3, turnEnded)
      output.writeTag(4, 2)
      output.writeRawVarint32(population.serializedSize)
      population.writeTo(output)
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.game.PlayerState = {
      var __resources = this.resources
      var __actions = this.actions
      var __turnEnded = this.turnEnded
      var __population = this.population
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 8 =>
            __resources = __input.readUInt32()
          case 16 =>
            __actions = __input.readUInt32()
          case 24 =>
            __turnEnded = __input.readBool()
          case 34 =>
            __population = com.trueaccord.scalapb.LiteParser.readMessage(__input, __population)
          case tag => __input.skipField(tag)
        }
      }
      netmsg.game.PlayerState(
          resources = __resources,
          actions = __actions,
          turnEnded = __turnEnded,
          population = __population
      )
    }
    def withResources(__v: Int): PlayerState = copy(resources = __v)
    def withActions(__v: Int): PlayerState = copy(actions = __v)
    def withTurnEnded(__v: Boolean): PlayerState = copy(turnEnded = __v)
    def withPopulation(__v: netmsg.base.ValWithMax): PlayerState = copy(population = __v)
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => resources
        case 2 => actions
        case 3 => turnEnded
        case 4 => population
      }
    }
    def companion = netmsg.game.PlayerState
}

object PlayerState extends com.trueaccord.scalapb.GeneratedMessageCompanion[PlayerState]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[PlayerState]  = this
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.game.PlayerState = netmsg.game.PlayerState(
    resources = fieldsMap(1).asInstanceOf[Int],
    actions = fieldsMap(2).asInstanceOf[Int],
    turnEnded = fieldsMap(3).asInstanceOf[Boolean],
    population = fieldsMap(4).asInstanceOf[netmsg.base.ValWithMax]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("PlayerState", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.game.InternalFields_game.internalFieldsFor("netmsg.game.PlayerState"))
  lazy val defaultInstance = netmsg.game.PlayerState(
    resources = 0,
    actions = 0,
    turnEnded = false,
    population = netmsg.base.ValWithMax.defaultInstance
  )
  implicit class PlayerStateLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, PlayerState]) extends com.trueaccord.lenses.ObjectLens[UpperPB, PlayerState](_l) {
    def resources: com.trueaccord.lenses.Lens[UpperPB, Int] = field(_.resources)((c_, f_) => c_.copy(resources = f_))
    def actions: com.trueaccord.lenses.Lens[UpperPB, Int] = field(_.actions)((c_, f_) => c_.copy(actions = f_))
    def turnEnded: com.trueaccord.lenses.Lens[UpperPB, Boolean] = field(_.turnEnded)((c_, f_) => c_.copy(turnEnded = f_))
    def population: com.trueaccord.lenses.Lens[UpperPB, netmsg.base.ValWithMax] = field(_.population)((c_, f_) => c_.copy(population = f_))
  }
  final val RESOURCES_FIELD_NUMBER = 1
  final val ACTIONS_FIELD_NUMBER = 2
  final val TURN_ENDED_FIELD_NUMBER = 3
  final val POPULATION_FIELD_NUMBER = 4
}