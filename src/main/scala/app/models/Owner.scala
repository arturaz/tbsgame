package app.models

import java.util.UUID

import app.models.world.OwnedObj

object Owner {
  type Id = UUID
  @inline def newId = UUID.randomUUID()
}

sealed trait Owner {
  def id: Owner.Id
  def team: Team
  
  def isFriendOf(other: Owner) = team == other.team
  def isEnemyOf(other: Owner) = ! isFriendOf(other)
}

case class Team(id: Owner.Id=Owner.newId) extends Owner {
  override def team = this
}

trait Player extends Owner {
  def isHuman: Boolean
  def asHuman: Option[Human]
  def isBot = ! isHuman
  def asBot: Option[Bot]
}

object Player {
  def unapply(fo: OwnedObj) = fo.owner match {
    case p: Player => Some(p)
    case _ => None
  }
}
case class Human(
  name: String, team: Team, id: Owner.Id=Owner.newId
) extends Player {
  override def isHuman = true
  override def asHuman = Some(this)
  override def asBot = None
}

case class Bot(team: Team, id: Owner.Id=Owner.newId) extends Player {
  override def isHuman = false
  override def asHuman = None
  override def asBot = Some(this)
}
