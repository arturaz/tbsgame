package app.models.game.world

import app.models.game.Owner
import app.models.game.events.{OwnerChangeEvt, Evented}

import implicits._

import scala.language.implicitConversions

trait RespawnsOnDestructionStats extends OwnedObjStats {
  def hpAfterRespawn: HP
}

trait RespawnsOnDestructionImpl extends OwnedObjImpl {
  val stats: RespawnsOnDestructionStats

  def ownerAfterRespawn(attacker: Owner): Owner
}

trait RespawnsOnDestructionOps[+Self <: RespawnsOnDestruction] {
  def self: Self

  protected def withNewOwner(owner: Owner): Self

  def withNewOwnerEvt(world: World, owner: Owner) = {
    val newObj = withNewOwner(owner)
    Evented(
      newObj,
      if (self.owner == newObj.owner) Vector.empty
      else Vector(OwnerChangeEvt(world, newObj))
    )
  }

  def respawnSelf(world: World, newOwner: Owner): Evented[Self] = {
    for {
      self <- self.withNewOwnerEvt(world, newOwner)
      self <- self.withNewHPEvt(self.stats.hpAfterRespawn)(world)
    } yield self
  }

  def respawn(world: World, newOwner: Owner): Evented[World] = {
    World.revealObjects(
      newOwner.team,
      respawnSelf(world, newOwner).flatMap(world.updated(self, _))
    )
  }
}

trait ToRespawnsOnDesctructionOps {
  implicit def toRespawnsOnDestructionOps[A <: RespawnsOnDestruction](a: A)
  : RespawnsOnDestructionOps[A] = (a match {

  }).asInstanceOf[RespawnsOnDestructionOps[A]]
}

object RespawnsOnDestruction extends ToRespawnsOnDesctructionOps