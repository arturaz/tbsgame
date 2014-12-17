package app.models.game.world

import app.models.game.Owner
import app.models.game.events.{OwnerChangeEvt, Evented}

import implicits._

trait RespawnsOnDestructionOps[
  Self <: RespawnsOnDestruction,
  OwnerType <: Owner
] extends OwnedObjOps[Self] {
  protected def withNewOwner(owner: OwnerType)(self: Self): Self
  def withNewOwnerEvt(world: World, owner: OwnerType)(self: Self) = {
    val newObj = self |> withNewOwner(owner)
    Evented(
      newObj,
      if (self.owner == newObj.owner) Vector.empty
      else Vector(OwnerChangeEvt(world, newObj))
    )
  }
}

trait RespawnsOnDestructionStats extends OwnedObjStats {
  def hpAfterRespawn: HP
}

trait RespawnsOnDestructionCompanion[
  Self <: RespawnsOnDestruction,
  OwnerType <: Owner
] extends OwnedObjCompanion[Self] with RespawnsOnDestructionOps[Self, OwnerType]
with RespawnsOnDestructionStats

/* If destroyed this object should respawn with new owner and hp. */
trait RespawnsOnDestruction extends OwnedObj {
  type OwnerType <: Owner
  type Self <: RespawnsOnDestruction
  type Companion <:
    RespawnsOnDestructionStats with RespawnsOnDestructionOps[Self, OwnerType]

  def ownerAfterRespawn(attacker: Owner): OwnerType

  def respawnSelf(world: World, newOwner: OwnerType): Evented[Self] = {
    for {
      self <- self |> companion.withNewOwnerEvt(world, newOwner)
      self <- companion.withNewHPEvt(companion.hpAfterRespawn)(world, self)
    } yield self
  }

  def respawn(world: World, newOwner: OwnerType): Evented[World] = {
    World.revealObjects(newOwner.team,
      respawnSelf(world, newOwner).flatMap(world.updated(self, _))
    )
  }
}
