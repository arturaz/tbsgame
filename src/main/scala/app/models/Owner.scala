package app.models

import app.models.game.ai.AIController

sealed trait Owner
sealed trait Player

class Team extends Owner
class AI(controllerF: AI => AIController) extends Owner with Player {
  val controller = controllerF(this)
}
class Human(name: String, team: Team) extends Owner with Player
