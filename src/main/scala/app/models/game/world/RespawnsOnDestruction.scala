package app.models.game.world

import app.models.game.Owner
import app.models.game.events.{Evented, OwnerChangeEvt}
import app.models.game.world.buildings.VPTowerOps

import scala.language.implicitConversions
import app.models.game.world.Ops._

trait RespawnsOnDestructionStats extends OwnedObjStats {
  def hpAfterRespawn: HP
}

trait RespawnsOnDestructionImpl extends OwnedObjImpl {
  type OwnerType <: Owner
  type Stats <:RespawnsOnDestructionStats

  def ownerAfterRespawn(attacker: Owner): OwnerType
}

trait RespawnsOnDestructionOps[Self <: RespawnsOnDestruction] extends OwnedObjOps[Self] {
  type OwnerType = Self#OwnerType

  protected def withNewOwner(owner: OwnerType): Self

  def withNewOwnerEvt(world: World, owner: OwnerType) = {
    val newObj = withNewOwner(owner)
    Evented(
      newObj,
      if (self.owner == newObj.owner) Vector.empty
      else Vector(OwnerChangeEvt(world, newObj))
    )
  }

  def respawnSelf(world: World, newOwner: OwnerType): Evented[Self] = {
    for {
      self <- self.withNewOwnerEvt(world, newOwner)
      self <- self.withNewHPEvt(self.stats.hpAfterRespawn)(world)
    } yield self
  }

  def respawn(world: World, newOwner: OwnerType): Evented[World] = {
    World.revealObjects(
      newOwner.team,
      respawnSelf(world, newOwner).flatMap(world.updated(self, _))
    )
  }
}

trait ToRespawnsOnDesctructionOps {
  implicit def toRespawnsOnDestructionOps[A <: RespawnsOnDestruction](a: A)
  : RespawnsOnDestructionOps[A] = (a match {
    case o: VPTower => VPTowerOps(o)
  }).asInstanceOf[RespawnsOnDestructionOps[A]]
}