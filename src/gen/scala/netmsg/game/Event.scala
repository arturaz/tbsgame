// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!

package netmsg.game


import com.trueaccord.scalapb.Descriptors

final case class Event(
    roundStarted: Option[netmsg.game.RoundStartedEvt] = None,
    turnStarted: Option[netmsg.game.TurnStartedEvt] = None,
    pointOwnerMapChange: Option[netmsg.game.PointOwnerMapChangeEvt] = None,
    warp: Option[netmsg.game.WarpEvt] = None,
    objVisible: Option[netmsg.game.ObjVisibleEvt] = None,
    move: Option[netmsg.game.MoveEvt] = None,
    attack: Option[netmsg.game.AttackEvt] = None,
    movementChange: Option[netmsg.game.MovementChangeEvt] = None,
    objAdded: Option[netmsg.game.ObjAddedEvt] = None,
    resourceChange: Option[netmsg.game.ResourceChangeEvt] = None,
    actionsChange: Option[netmsg.game.ActionsChangeEvt] = None,
    warpChange: Option[netmsg.game.WarpStateChangeEvt] = None,
    attacksChange: Option[netmsg.game.AttacksChangeEvt] = None,
    waitingForRoundEndChange: Option[netmsg.game.WaitingForRoundEndChangeEvt] = None,
    objDestroyed: Option[netmsg.game.ObjDestroyedEvt] = None,
    hpChange: Option[netmsg.game.HPChangeEvt] = None,
    levelChange: Option[netmsg.game.LevelChangeEvt] = None,
    ownerChange: Option[netmsg.game.OwnerChangeEvt] = None,
    objectivesUpdate: Option[netmsg.game.ObjectivesUpdateEvt] = None,
    populationChange: Option[netmsg.game.PopulationChangeEvt] = None,
    attackPos: Option[netmsg.game.AttackPosEvt] = None,
    join: Option[netmsg.game.JoinEvt] = None,
    leave: Option[netmsg.game.LeaveEvt] = None,
    gameWon: Option[netmsg.game.GameWonEvt] = None
    ) extends com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[Event] with com.trueaccord.lenses.Updatable[Event] {
    lazy val serializedSize: Int = {
      var __size = 0
      if (roundStarted.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(roundStarted.get.serializedSize) + roundStarted.get.serializedSize }
      if (turnStarted.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(turnStarted.get.serializedSize) + turnStarted.get.serializedSize }
      if (pointOwnerMapChange.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(pointOwnerMapChange.get.serializedSize) + pointOwnerMapChange.get.serializedSize }
      if (warp.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(warp.get.serializedSize) + warp.get.serializedSize }
      if (objVisible.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(objVisible.get.serializedSize) + objVisible.get.serializedSize }
      if (move.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(move.get.serializedSize) + move.get.serializedSize }
      if (attack.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(attack.get.serializedSize) + attack.get.serializedSize }
      if (movementChange.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(movementChange.get.serializedSize) + movementChange.get.serializedSize }
      if (objAdded.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(objAdded.get.serializedSize) + objAdded.get.serializedSize }
      if (resourceChange.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(resourceChange.get.serializedSize) + resourceChange.get.serializedSize }
      if (actionsChange.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(actionsChange.get.serializedSize) + actionsChange.get.serializedSize }
      if (warpChange.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(warpChange.get.serializedSize) + warpChange.get.serializedSize }
      if (attacksChange.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(attacksChange.get.serializedSize) + attacksChange.get.serializedSize }
      if (waitingForRoundEndChange.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(waitingForRoundEndChange.get.serializedSize) + waitingForRoundEndChange.get.serializedSize }
      if (objDestroyed.isDefined) { __size += 1 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(objDestroyed.get.serializedSize) + objDestroyed.get.serializedSize }
      if (hpChange.isDefined) { __size += 2 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(hpChange.get.serializedSize) + hpChange.get.serializedSize }
      if (levelChange.isDefined) { __size += 2 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(levelChange.get.serializedSize) + levelChange.get.serializedSize }
      if (ownerChange.isDefined) { __size += 2 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(ownerChange.get.serializedSize) + ownerChange.get.serializedSize }
      if (objectivesUpdate.isDefined) { __size += 2 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(objectivesUpdate.get.serializedSize) + objectivesUpdate.get.serializedSize }
      if (populationChange.isDefined) { __size += 2 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(populationChange.get.serializedSize) + populationChange.get.serializedSize }
      if (attackPos.isDefined) { __size += 2 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(attackPos.get.serializedSize) + attackPos.get.serializedSize }
      if (join.isDefined) { __size += 2 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(join.get.serializedSize) + join.get.serializedSize }
      if (leave.isDefined) { __size += 2 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(leave.get.serializedSize) + leave.get.serializedSize }
      if (gameWon.isDefined) { __size += 2 + com.google.protobuf.CodedOutputStream.computeRawVarint32Size(gameWon.get.serializedSize) + gameWon.get.serializedSize }
      __size
    }
    def writeTo(output: com.google.protobuf.CodedOutputStream): Unit = {
      roundStarted.foreach { v => 
        output.writeTag(1, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      turnStarted.foreach { v => 
        output.writeTag(2, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      pointOwnerMapChange.foreach { v => 
        output.writeTag(3, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      warp.foreach { v => 
        output.writeTag(4, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      objVisible.foreach { v => 
        output.writeTag(5, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      move.foreach { v => 
        output.writeTag(6, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      attack.foreach { v => 
        output.writeTag(7, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      movementChange.foreach { v => 
        output.writeTag(8, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      objAdded.foreach { v => 
        output.writeTag(9, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      resourceChange.foreach { v => 
        output.writeTag(10, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      actionsChange.foreach { v => 
        output.writeTag(11, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      warpChange.foreach { v => 
        output.writeTag(12, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      attacksChange.foreach { v => 
        output.writeTag(13, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      waitingForRoundEndChange.foreach { v => 
        output.writeTag(14, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      objDestroyed.foreach { v => 
        output.writeTag(15, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      hpChange.foreach { v => 
        output.writeTag(16, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      levelChange.foreach { v => 
        output.writeTag(17, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      ownerChange.foreach { v => 
        output.writeTag(18, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      objectivesUpdate.foreach { v => 
        output.writeTag(19, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      populationChange.foreach { v => 
        output.writeTag(20, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      attackPos.foreach { v => 
        output.writeTag(21, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      join.foreach { v => 
        output.writeTag(1000, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      leave.foreach { v => 
        output.writeTag(1001, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
      gameWon.foreach { v => 
        output.writeTag(1002, 2)
        output.writeRawVarint32(v.serializedSize)
        v.writeTo(output)
      }
    }
    def mergeFrom(__input: com.google.protobuf.CodedInputStream): netmsg.game.Event = {
      var __roundStarted = this.roundStarted
      var __turnStarted = this.turnStarted
      var __pointOwnerMapChange = this.pointOwnerMapChange
      var __warp = this.warp
      var __objVisible = this.objVisible
      var __move = this.move
      var __attack = this.attack
      var __movementChange = this.movementChange
      var __objAdded = this.objAdded
      var __resourceChange = this.resourceChange
      var __actionsChange = this.actionsChange
      var __warpChange = this.warpChange
      var __attacksChange = this.attacksChange
      var __waitingForRoundEndChange = this.waitingForRoundEndChange
      var __objDestroyed = this.objDestroyed
      var __hpChange = this.hpChange
      var __levelChange = this.levelChange
      var __ownerChange = this.ownerChange
      var __objectivesUpdate = this.objectivesUpdate
      var __populationChange = this.populationChange
      var __attackPos = this.attackPos
      var __join = this.join
      var __leave = this.leave
      var __gameWon = this.gameWon
      var _done__ = false
      while (!_done__) {
        val _tag__ = __input.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __roundStarted = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __roundStarted.getOrElse(netmsg.game.RoundStartedEvt.defaultInstance)))
          case 18 =>
            __turnStarted = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __turnStarted.getOrElse(netmsg.game.TurnStartedEvt.defaultInstance)))
          case 26 =>
            __pointOwnerMapChange = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __pointOwnerMapChange.getOrElse(netmsg.game.PointOwnerMapChangeEvt.defaultInstance)))
          case 34 =>
            __warp = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __warp.getOrElse(netmsg.game.WarpEvt.defaultInstance)))
          case 42 =>
            __objVisible = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __objVisible.getOrElse(netmsg.game.ObjVisibleEvt.defaultInstance)))
          case 50 =>
            __move = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __move.getOrElse(netmsg.game.MoveEvt.defaultInstance)))
          case 58 =>
            __attack = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __attack.getOrElse(netmsg.game.AttackEvt.defaultInstance)))
          case 66 =>
            __movementChange = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __movementChange.getOrElse(netmsg.game.MovementChangeEvt.defaultInstance)))
          case 74 =>
            __objAdded = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __objAdded.getOrElse(netmsg.game.ObjAddedEvt.defaultInstance)))
          case 82 =>
            __resourceChange = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __resourceChange.getOrElse(netmsg.game.ResourceChangeEvt.defaultInstance)))
          case 90 =>
            __actionsChange = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __actionsChange.getOrElse(netmsg.game.ActionsChangeEvt.defaultInstance)))
          case 98 =>
            __warpChange = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __warpChange.getOrElse(netmsg.game.WarpStateChangeEvt.defaultInstance)))
          case 106 =>
            __attacksChange = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __attacksChange.getOrElse(netmsg.game.AttacksChangeEvt.defaultInstance)))
          case 114 =>
            __waitingForRoundEndChange = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __waitingForRoundEndChange.getOrElse(netmsg.game.WaitingForRoundEndChangeEvt.defaultInstance)))
          case 122 =>
            __objDestroyed = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __objDestroyed.getOrElse(netmsg.game.ObjDestroyedEvt.defaultInstance)))
          case 130 =>
            __hpChange = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __hpChange.getOrElse(netmsg.game.HPChangeEvt.defaultInstance)))
          case 138 =>
            __levelChange = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __levelChange.getOrElse(netmsg.game.LevelChangeEvt.defaultInstance)))
          case 146 =>
            __ownerChange = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __ownerChange.getOrElse(netmsg.game.OwnerChangeEvt.defaultInstance)))
          case 154 =>
            __objectivesUpdate = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __objectivesUpdate.getOrElse(netmsg.game.ObjectivesUpdateEvt.defaultInstance)))
          case 162 =>
            __populationChange = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __populationChange.getOrElse(netmsg.game.PopulationChangeEvt.defaultInstance)))
          case 170 =>
            __attackPos = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __attackPos.getOrElse(netmsg.game.AttackPosEvt.defaultInstance)))
          case 8002 =>
            __join = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __join.getOrElse(netmsg.game.JoinEvt.defaultInstance)))
          case 8010 =>
            __leave = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __leave.getOrElse(netmsg.game.LeaveEvt.defaultInstance)))
          case 8018 =>
            __gameWon = Some(com.trueaccord.scalapb.LiteParser.readMessage(__input, __gameWon.getOrElse(netmsg.game.GameWonEvt.defaultInstance)))
          case tag => __input.skipField(tag)
        }
      }
      netmsg.game.Event(
          roundStarted = __roundStarted,
          turnStarted = __turnStarted,
          pointOwnerMapChange = __pointOwnerMapChange,
          warp = __warp,
          objVisible = __objVisible,
          move = __move,
          attack = __attack,
          movementChange = __movementChange,
          objAdded = __objAdded,
          resourceChange = __resourceChange,
          actionsChange = __actionsChange,
          warpChange = __warpChange,
          attacksChange = __attacksChange,
          waitingForRoundEndChange = __waitingForRoundEndChange,
          objDestroyed = __objDestroyed,
          hpChange = __hpChange,
          levelChange = __levelChange,
          ownerChange = __ownerChange,
          objectivesUpdate = __objectivesUpdate,
          populationChange = __populationChange,
          attackPos = __attackPos,
          join = __join,
          leave = __leave,
          gameWon = __gameWon
      )
    }
    def getRoundStarted: netmsg.game.RoundStartedEvt = roundStarted.getOrElse(netmsg.game.RoundStartedEvt.defaultInstance)
    def clearRoundStarted: Event = copy(roundStarted = None)
    def withRoundStarted(__v: netmsg.game.RoundStartedEvt): Event = copy(roundStarted = Some(__v))
    def getTurnStarted: netmsg.game.TurnStartedEvt = turnStarted.getOrElse(netmsg.game.TurnStartedEvt.defaultInstance)
    def clearTurnStarted: Event = copy(turnStarted = None)
    def withTurnStarted(__v: netmsg.game.TurnStartedEvt): Event = copy(turnStarted = Some(__v))
    def getPointOwnerMapChange: netmsg.game.PointOwnerMapChangeEvt = pointOwnerMapChange.getOrElse(netmsg.game.PointOwnerMapChangeEvt.defaultInstance)
    def clearPointOwnerMapChange: Event = copy(pointOwnerMapChange = None)
    def withPointOwnerMapChange(__v: netmsg.game.PointOwnerMapChangeEvt): Event = copy(pointOwnerMapChange = Some(__v))
    def getWarp: netmsg.game.WarpEvt = warp.getOrElse(netmsg.game.WarpEvt.defaultInstance)
    def clearWarp: Event = copy(warp = None)
    def withWarp(__v: netmsg.game.WarpEvt): Event = copy(warp = Some(__v))
    def getObjVisible: netmsg.game.ObjVisibleEvt = objVisible.getOrElse(netmsg.game.ObjVisibleEvt.defaultInstance)
    def clearObjVisible: Event = copy(objVisible = None)
    def withObjVisible(__v: netmsg.game.ObjVisibleEvt): Event = copy(objVisible = Some(__v))
    def getMove: netmsg.game.MoveEvt = move.getOrElse(netmsg.game.MoveEvt.defaultInstance)
    def clearMove: Event = copy(move = None)
    def withMove(__v: netmsg.game.MoveEvt): Event = copy(move = Some(__v))
    def getAttack: netmsg.game.AttackEvt = attack.getOrElse(netmsg.game.AttackEvt.defaultInstance)
    def clearAttack: Event = copy(attack = None)
    def withAttack(__v: netmsg.game.AttackEvt): Event = copy(attack = Some(__v))
    def getMovementChange: netmsg.game.MovementChangeEvt = movementChange.getOrElse(netmsg.game.MovementChangeEvt.defaultInstance)
    def clearMovementChange: Event = copy(movementChange = None)
    def withMovementChange(__v: netmsg.game.MovementChangeEvt): Event = copy(movementChange = Some(__v))
    def getObjAdded: netmsg.game.ObjAddedEvt = objAdded.getOrElse(netmsg.game.ObjAddedEvt.defaultInstance)
    def clearObjAdded: Event = copy(objAdded = None)
    def withObjAdded(__v: netmsg.game.ObjAddedEvt): Event = copy(objAdded = Some(__v))
    def getResourceChange: netmsg.game.ResourceChangeEvt = resourceChange.getOrElse(netmsg.game.ResourceChangeEvt.defaultInstance)
    def clearResourceChange: Event = copy(resourceChange = None)
    def withResourceChange(__v: netmsg.game.ResourceChangeEvt): Event = copy(resourceChange = Some(__v))
    def getActionsChange: netmsg.game.ActionsChangeEvt = actionsChange.getOrElse(netmsg.game.ActionsChangeEvt.defaultInstance)
    def clearActionsChange: Event = copy(actionsChange = None)
    def withActionsChange(__v: netmsg.game.ActionsChangeEvt): Event = copy(actionsChange = Some(__v))
    def getWarpChange: netmsg.game.WarpStateChangeEvt = warpChange.getOrElse(netmsg.game.WarpStateChangeEvt.defaultInstance)
    def clearWarpChange: Event = copy(warpChange = None)
    def withWarpChange(__v: netmsg.game.WarpStateChangeEvt): Event = copy(warpChange = Some(__v))
    def getAttacksChange: netmsg.game.AttacksChangeEvt = attacksChange.getOrElse(netmsg.game.AttacksChangeEvt.defaultInstance)
    def clearAttacksChange: Event = copy(attacksChange = None)
    def withAttacksChange(__v: netmsg.game.AttacksChangeEvt): Event = copy(attacksChange = Some(__v))
    def getWaitingForRoundEndChange: netmsg.game.WaitingForRoundEndChangeEvt = waitingForRoundEndChange.getOrElse(netmsg.game.WaitingForRoundEndChangeEvt.defaultInstance)
    def clearWaitingForRoundEndChange: Event = copy(waitingForRoundEndChange = None)
    def withWaitingForRoundEndChange(__v: netmsg.game.WaitingForRoundEndChangeEvt): Event = copy(waitingForRoundEndChange = Some(__v))
    def getObjDestroyed: netmsg.game.ObjDestroyedEvt = objDestroyed.getOrElse(netmsg.game.ObjDestroyedEvt.defaultInstance)
    def clearObjDestroyed: Event = copy(objDestroyed = None)
    def withObjDestroyed(__v: netmsg.game.ObjDestroyedEvt): Event = copy(objDestroyed = Some(__v))
    def getHpChange: netmsg.game.HPChangeEvt = hpChange.getOrElse(netmsg.game.HPChangeEvt.defaultInstance)
    def clearHpChange: Event = copy(hpChange = None)
    def withHpChange(__v: netmsg.game.HPChangeEvt): Event = copy(hpChange = Some(__v))
    def getLevelChange: netmsg.game.LevelChangeEvt = levelChange.getOrElse(netmsg.game.LevelChangeEvt.defaultInstance)
    def clearLevelChange: Event = copy(levelChange = None)
    def withLevelChange(__v: netmsg.game.LevelChangeEvt): Event = copy(levelChange = Some(__v))
    def getOwnerChange: netmsg.game.OwnerChangeEvt = ownerChange.getOrElse(netmsg.game.OwnerChangeEvt.defaultInstance)
    def clearOwnerChange: Event = copy(ownerChange = None)
    def withOwnerChange(__v: netmsg.game.OwnerChangeEvt): Event = copy(ownerChange = Some(__v))
    def getObjectivesUpdate: netmsg.game.ObjectivesUpdateEvt = objectivesUpdate.getOrElse(netmsg.game.ObjectivesUpdateEvt.defaultInstance)
    def clearObjectivesUpdate: Event = copy(objectivesUpdate = None)
    def withObjectivesUpdate(__v: netmsg.game.ObjectivesUpdateEvt): Event = copy(objectivesUpdate = Some(__v))
    def getPopulationChange: netmsg.game.PopulationChangeEvt = populationChange.getOrElse(netmsg.game.PopulationChangeEvt.defaultInstance)
    def clearPopulationChange: Event = copy(populationChange = None)
    def withPopulationChange(__v: netmsg.game.PopulationChangeEvt): Event = copy(populationChange = Some(__v))
    def getAttackPos: netmsg.game.AttackPosEvt = attackPos.getOrElse(netmsg.game.AttackPosEvt.defaultInstance)
    def clearAttackPos: Event = copy(attackPos = None)
    def withAttackPos(__v: netmsg.game.AttackPosEvt): Event = copy(attackPos = Some(__v))
    def getJoin: netmsg.game.JoinEvt = join.getOrElse(netmsg.game.JoinEvt.defaultInstance)
    def clearJoin: Event = copy(join = None)
    def withJoin(__v: netmsg.game.JoinEvt): Event = copy(join = Some(__v))
    def getLeave: netmsg.game.LeaveEvt = leave.getOrElse(netmsg.game.LeaveEvt.defaultInstance)
    def clearLeave: Event = copy(leave = None)
    def withLeave(__v: netmsg.game.LeaveEvt): Event = copy(leave = Some(__v))
    def getGameWon: netmsg.game.GameWonEvt = gameWon.getOrElse(netmsg.game.GameWonEvt.defaultInstance)
    def clearGameWon: Event = copy(gameWon = None)
    def withGameWon(__v: netmsg.game.GameWonEvt): Event = copy(gameWon = Some(__v))
    def getField(__field: Descriptors.FieldDescriptor): Any = {
      __field.number match {
        case 1 => roundStarted
        case 2 => turnStarted
        case 3 => pointOwnerMapChange
        case 4 => warp
        case 5 => objVisible
        case 6 => move
        case 7 => attack
        case 8 => movementChange
        case 9 => objAdded
        case 10 => resourceChange
        case 11 => actionsChange
        case 12 => warpChange
        case 13 => attacksChange
        case 14 => waitingForRoundEndChange
        case 15 => objDestroyed
        case 16 => hpChange
        case 17 => levelChange
        case 18 => ownerChange
        case 19 => objectivesUpdate
        case 20 => populationChange
        case 21 => attackPos
        case 1000 => join
        case 1001 => leave
        case 1002 => gameWon
      }
    }
    def companion = netmsg.game.Event
}

object Event extends com.trueaccord.scalapb.GeneratedMessageCompanion[Event]  {
  implicit def messageCompanion: com.trueaccord.scalapb.GeneratedMessageCompanion[Event]  = this
  def fromFieldsMap(fieldsMap: Map[Int, Any]): netmsg.game.Event = netmsg.game.Event(
    roundStarted = fieldsMap.getOrElse(1, None).asInstanceOf[Option[netmsg.game.RoundStartedEvt]],
    turnStarted = fieldsMap.getOrElse(2, None).asInstanceOf[Option[netmsg.game.TurnStartedEvt]],
    pointOwnerMapChange = fieldsMap.getOrElse(3, None).asInstanceOf[Option[netmsg.game.PointOwnerMapChangeEvt]],
    warp = fieldsMap.getOrElse(4, None).asInstanceOf[Option[netmsg.game.WarpEvt]],
    objVisible = fieldsMap.getOrElse(5, None).asInstanceOf[Option[netmsg.game.ObjVisibleEvt]],
    move = fieldsMap.getOrElse(6, None).asInstanceOf[Option[netmsg.game.MoveEvt]],
    attack = fieldsMap.getOrElse(7, None).asInstanceOf[Option[netmsg.game.AttackEvt]],
    movementChange = fieldsMap.getOrElse(8, None).asInstanceOf[Option[netmsg.game.MovementChangeEvt]],
    objAdded = fieldsMap.getOrElse(9, None).asInstanceOf[Option[netmsg.game.ObjAddedEvt]],
    resourceChange = fieldsMap.getOrElse(10, None).asInstanceOf[Option[netmsg.game.ResourceChangeEvt]],
    actionsChange = fieldsMap.getOrElse(11, None).asInstanceOf[Option[netmsg.game.ActionsChangeEvt]],
    warpChange = fieldsMap.getOrElse(12, None).asInstanceOf[Option[netmsg.game.WarpStateChangeEvt]],
    attacksChange = fieldsMap.getOrElse(13, None).asInstanceOf[Option[netmsg.game.AttacksChangeEvt]],
    waitingForRoundEndChange = fieldsMap.getOrElse(14, None).asInstanceOf[Option[netmsg.game.WaitingForRoundEndChangeEvt]],
    objDestroyed = fieldsMap.getOrElse(15, None).asInstanceOf[Option[netmsg.game.ObjDestroyedEvt]],
    hpChange = fieldsMap.getOrElse(16, None).asInstanceOf[Option[netmsg.game.HPChangeEvt]],
    levelChange = fieldsMap.getOrElse(17, None).asInstanceOf[Option[netmsg.game.LevelChangeEvt]],
    ownerChange = fieldsMap.getOrElse(18, None).asInstanceOf[Option[netmsg.game.OwnerChangeEvt]],
    objectivesUpdate = fieldsMap.getOrElse(19, None).asInstanceOf[Option[netmsg.game.ObjectivesUpdateEvt]],
    populationChange = fieldsMap.getOrElse(20, None).asInstanceOf[Option[netmsg.game.PopulationChangeEvt]],
    attackPos = fieldsMap.getOrElse(21, None).asInstanceOf[Option[netmsg.game.AttackPosEvt]],
    join = fieldsMap.getOrElse(1000, None).asInstanceOf[Option[netmsg.game.JoinEvt]],
    leave = fieldsMap.getOrElse(1001, None).asInstanceOf[Option[netmsg.game.LeaveEvt]],
    gameWon = fieldsMap.getOrElse(1002, None).asInstanceOf[Option[netmsg.game.GameWonEvt]]
  )
  lazy val descriptor = new Descriptors.MessageDescriptor("Event", this,
    None, m = Seq(),
    e = Seq(),
    f = netmsg.game.InternalFields_game.internalFieldsFor("netmsg.game.Event"))
  lazy val defaultInstance = netmsg.game.Event(
  )
  implicit class EventLens[UpperPB](_l: com.trueaccord.lenses.Lens[UpperPB, Event]) extends com.trueaccord.lenses.ObjectLens[UpperPB, Event](_l) {
    def roundStarted: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.RoundStartedEvt] = field(_.getRoundStarted)((c_, f_) => c_.copy(roundStarted = Some(f_)))
    def optionalRoundStarted: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.RoundStartedEvt]] = field(_.roundStarted)((c_, f_) => c_.copy(roundStarted = f_))
    def turnStarted: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.TurnStartedEvt] = field(_.getTurnStarted)((c_, f_) => c_.copy(turnStarted = Some(f_)))
    def optionalTurnStarted: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.TurnStartedEvt]] = field(_.turnStarted)((c_, f_) => c_.copy(turnStarted = f_))
    def pointOwnerMapChange: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.PointOwnerMapChangeEvt] = field(_.getPointOwnerMapChange)((c_, f_) => c_.copy(pointOwnerMapChange = Some(f_)))
    def optionalPointOwnerMapChange: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.PointOwnerMapChangeEvt]] = field(_.pointOwnerMapChange)((c_, f_) => c_.copy(pointOwnerMapChange = f_))
    def warp: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WarpEvt] = field(_.getWarp)((c_, f_) => c_.copy(warp = Some(f_)))
    def optionalWarp: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.WarpEvt]] = field(_.warp)((c_, f_) => c_.copy(warp = f_))
    def objVisible: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.ObjVisibleEvt] = field(_.getObjVisible)((c_, f_) => c_.copy(objVisible = Some(f_)))
    def optionalObjVisible: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.ObjVisibleEvt]] = field(_.objVisible)((c_, f_) => c_.copy(objVisible = f_))
    def move: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.MoveEvt] = field(_.getMove)((c_, f_) => c_.copy(move = Some(f_)))
    def optionalMove: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.MoveEvt]] = field(_.move)((c_, f_) => c_.copy(move = f_))
    def attack: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.AttackEvt] = field(_.getAttack)((c_, f_) => c_.copy(attack = Some(f_)))
    def optionalAttack: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.AttackEvt]] = field(_.attack)((c_, f_) => c_.copy(attack = f_))
    def movementChange: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.MovementChangeEvt] = field(_.getMovementChange)((c_, f_) => c_.copy(movementChange = Some(f_)))
    def optionalMovementChange: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.MovementChangeEvt]] = field(_.movementChange)((c_, f_) => c_.copy(movementChange = f_))
    def objAdded: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.ObjAddedEvt] = field(_.getObjAdded)((c_, f_) => c_.copy(objAdded = Some(f_)))
    def optionalObjAdded: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.ObjAddedEvt]] = field(_.objAdded)((c_, f_) => c_.copy(objAdded = f_))
    def resourceChange: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.ResourceChangeEvt] = field(_.getResourceChange)((c_, f_) => c_.copy(resourceChange = Some(f_)))
    def optionalResourceChange: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.ResourceChangeEvt]] = field(_.resourceChange)((c_, f_) => c_.copy(resourceChange = f_))
    def actionsChange: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.ActionsChangeEvt] = field(_.getActionsChange)((c_, f_) => c_.copy(actionsChange = Some(f_)))
    def optionalActionsChange: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.ActionsChangeEvt]] = field(_.actionsChange)((c_, f_) => c_.copy(actionsChange = f_))
    def warpChange: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WarpStateChangeEvt] = field(_.getWarpChange)((c_, f_) => c_.copy(warpChange = Some(f_)))
    def optionalWarpChange: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.WarpStateChangeEvt]] = field(_.warpChange)((c_, f_) => c_.copy(warpChange = f_))
    def attacksChange: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.AttacksChangeEvt] = field(_.getAttacksChange)((c_, f_) => c_.copy(attacksChange = Some(f_)))
    def optionalAttacksChange: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.AttacksChangeEvt]] = field(_.attacksChange)((c_, f_) => c_.copy(attacksChange = f_))
    def waitingForRoundEndChange: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.WaitingForRoundEndChangeEvt] = field(_.getWaitingForRoundEndChange)((c_, f_) => c_.copy(waitingForRoundEndChange = Some(f_)))
    def optionalWaitingForRoundEndChange: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.WaitingForRoundEndChangeEvt]] = field(_.waitingForRoundEndChange)((c_, f_) => c_.copy(waitingForRoundEndChange = f_))
    def objDestroyed: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.ObjDestroyedEvt] = field(_.getObjDestroyed)((c_, f_) => c_.copy(objDestroyed = Some(f_)))
    def optionalObjDestroyed: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.ObjDestroyedEvt]] = field(_.objDestroyed)((c_, f_) => c_.copy(objDestroyed = f_))
    def hpChange: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.HPChangeEvt] = field(_.getHpChange)((c_, f_) => c_.copy(hpChange = Some(f_)))
    def optionalHpChange: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.HPChangeEvt]] = field(_.hpChange)((c_, f_) => c_.copy(hpChange = f_))
    def levelChange: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.LevelChangeEvt] = field(_.getLevelChange)((c_, f_) => c_.copy(levelChange = Some(f_)))
    def optionalLevelChange: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.LevelChangeEvt]] = field(_.levelChange)((c_, f_) => c_.copy(levelChange = f_))
    def ownerChange: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.OwnerChangeEvt] = field(_.getOwnerChange)((c_, f_) => c_.copy(ownerChange = Some(f_)))
    def optionalOwnerChange: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.OwnerChangeEvt]] = field(_.ownerChange)((c_, f_) => c_.copy(ownerChange = f_))
    def objectivesUpdate: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.ObjectivesUpdateEvt] = field(_.getObjectivesUpdate)((c_, f_) => c_.copy(objectivesUpdate = Some(f_)))
    def optionalObjectivesUpdate: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.ObjectivesUpdateEvt]] = field(_.objectivesUpdate)((c_, f_) => c_.copy(objectivesUpdate = f_))
    def populationChange: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.PopulationChangeEvt] = field(_.getPopulationChange)((c_, f_) => c_.copy(populationChange = Some(f_)))
    def optionalPopulationChange: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.PopulationChangeEvt]] = field(_.populationChange)((c_, f_) => c_.copy(populationChange = f_))
    def attackPos: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.AttackPosEvt] = field(_.getAttackPos)((c_, f_) => c_.copy(attackPos = Some(f_)))
    def optionalAttackPos: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.AttackPosEvt]] = field(_.attackPos)((c_, f_) => c_.copy(attackPos = f_))
    def join: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.JoinEvt] = field(_.getJoin)((c_, f_) => c_.copy(join = Some(f_)))
    def optionalJoin: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.JoinEvt]] = field(_.join)((c_, f_) => c_.copy(join = f_))
    def leave: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.LeaveEvt] = field(_.getLeave)((c_, f_) => c_.copy(leave = Some(f_)))
    def optionalLeave: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.LeaveEvt]] = field(_.leave)((c_, f_) => c_.copy(leave = f_))
    def gameWon: com.trueaccord.lenses.Lens[UpperPB, netmsg.game.GameWonEvt] = field(_.getGameWon)((c_, f_) => c_.copy(gameWon = Some(f_)))
    def optionalGameWon: com.trueaccord.lenses.Lens[UpperPB, Option[netmsg.game.GameWonEvt]] = field(_.gameWon)((c_, f_) => c_.copy(gameWon = f_))
  }
  final val ROUND_STARTED_FIELD_NUMBER = 1
  final val TURN_STARTED_FIELD_NUMBER = 2
  final val POINT_OWNER_MAP_CHANGE_FIELD_NUMBER = 3
  final val WARP_FIELD_NUMBER = 4
  final val OBJ_VISIBLE_FIELD_NUMBER = 5
  final val MOVE_FIELD_NUMBER = 6
  final val ATTACK_FIELD_NUMBER = 7
  final val MOVEMENT_CHANGE_FIELD_NUMBER = 8
  final val OBJ_ADDED_FIELD_NUMBER = 9
  final val RESOURCE_CHANGE_FIELD_NUMBER = 10
  final val ACTIONS_CHANGE_FIELD_NUMBER = 11
  final val WARP_CHANGE_FIELD_NUMBER = 12
  final val ATTACKS_CHANGE_FIELD_NUMBER = 13
  final val WAITING_FOR_ROUND_END_CHANGE_FIELD_NUMBER = 14
  final val OBJ_DESTROYED_FIELD_NUMBER = 15
  final val HP_CHANGE_FIELD_NUMBER = 16
  final val LEVEL_CHANGE_FIELD_NUMBER = 17
  final val OWNER_CHANGE_FIELD_NUMBER = 18
  final val OBJECTIVES_UPDATE_FIELD_NUMBER = 19
  final val POPULATION_CHANGE_FIELD_NUMBER = 20
  final val ATTACK_POS_FIELD_NUMBER = 21
  final val JOIN_FIELD_NUMBER = 1000
  final val LEAVE_FIELD_NUMBER = 1001
  final val GAME_WON_FIELD_NUMBER = 1002
}
