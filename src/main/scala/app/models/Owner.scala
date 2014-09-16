package app.models

sealed trait Owner {
  def team: Team
}

case class Team(id: Int) extends Owner {
  override def team = this
}
case class Player(id: Int, name: String, team: Team) extends Owner
