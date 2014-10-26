package app.models.game

import java.util.UUID

import app.models.User
import app.models.game.world.OwnedObj
import implicits._
import utils.IdObj

object Owner {
  sealed trait Id extends Any with IdObj
}

sealed trait Owner {
  def id: Owner.Id
  def team: Team
  
  def isFriendOf(other: Owner) = team === other.team
  def isEnemyOf(other: Owner) = ! isFriendOf(other)
}

object Team {
  case class Id(id: UUID) extends AnyVal with Owner.Id {
    override protected def prefix = "TID"
  }
  @inline def newId = Id(UUID.randomUUID())
}
case class Team(id: Team.Id=Team.newId) extends Owner {
  override def team = this
}

object Player {
  case class Id(id: UUID) extends AnyVal with Owner.Id {
    override protected def prefix = "PID"
  }
  @inline def newId = Id(UUID.randomUUID())

  def unapply(fo: OwnedObj) = fo.owner match {
    case p: Player => Some(p)
    case _ => None
  }
}
trait Player extends Owner {
  def id: Player.Id
  def isHuman: Boolean
  def asHuman: Option[Human]
  def isBot = ! isHuman
  def asBot: Option[Bot]
}

object Human {
  def apply(user: User, team: Team): Human = apply(user.name, team, Player.Id(user.id))
}
case class Human(
  name: String, team: Team, id: Player.Id=Player.newId
) extends Player {
  override def isHuman = true
  override def asHuman = Some(this)
  override def asBot = None
}

case class Bot(team: Team, id: Player.Id=Player.newId) extends Player {
  override def isHuman = false
  override def asHuman = None
  override def asBot = Some(this)
}
