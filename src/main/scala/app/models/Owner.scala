package app.models

import java.util.UUID

import app.models.world.OwnedObj

object Owner {
  type Id = UUID
  @inline def newId = UUID.randomUUID()
}

sealed trait Owner {
  def team: Team
  
  def isFriendOf(other: Owner) = team == other.team
  def isEnemyOf(other: Owner) = ! isFriendOf(other)
}

case class Team(id: Owner.Id=Owner.newId) extends Owner {
  override def team = this
}
object Player {
  def unapply(fo: OwnedObj) = fo.owner match {
    case p: Player => Some(p)
    case _ => None
  }
}
case class Player(
  name: String, team: Team, id: Owner.Id=Owner.newId
) extends Owner
