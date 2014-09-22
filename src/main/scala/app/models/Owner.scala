package app.models

import app.models.world.OwnedObj

sealed trait Owner {
  def team: Team
}

case class Team(id: Int) extends Owner {
  override def team = this
}
object Player {
  type Id = Int

  def unapply(fo: OwnedObj) = fo.owner match {
    case p: Player => Some(p)
    case _ => None
  }
}
case class Player(id: Player.Id, name: String, team: Team) extends Owner
