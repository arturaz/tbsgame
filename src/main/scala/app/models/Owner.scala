package app.models

import app.models.world.FactionObj

sealed trait Owner {
  def team: Team
}

case class Team(id: Int) extends Owner {
  override def team = this
}
object Player {
  def unapply(fo: FactionObj) = fo.owner match {
    case p: Player => Some(p)
    case _ => None
  }
}
case class Player(id: Int, name: String, team: Team) extends Owner
