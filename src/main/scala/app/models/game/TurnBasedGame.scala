package app.models.game

import app.models.game.events.Evented
import app.models.world.{World, Vect2, Warpable, WarpableCompanion}
import app.models.{Player, Team}
import implicits._

object TurnBasedGame {
  type StartedGame = Either[String, Evented[TurnBasedGame]]

  def apply(world: World, startingResources: Int): StartedGame =
    apply(Game(world, startingResources))

  def apply(game: Game): StartedGame = {
    val teams = game.world.teams.toVector
    if (teams.isEmpty) s"No teams found in $game!".left
    else newGameTurn(game, teams).right
  }

  private def newGameTurn(game: Game, teams: Vector[Team]): Evented[TurnBasedGame] = {
    val startingTeam = teams.head
    game.gameTurnStarted.flatMap(_.teamTurnStarted(startingTeam)).map {
      apply(_, startingTeam, teams.tail, Vector.empty)
    }
  }
}

case class TurnBasedGame private (
  game: Game, currentTeam: Team, readyTeams: Vector[Team], actedTeams: Vector[Team]
) extends GameLike[TurnBasedGame] {
  override def warp(player: Player, position: Vect2, warpable: WarpableCompanion[_ <: Warpable]) = ???
  override def move(player: Player, from: Vect2, to: Vect2) = ???
  override def special(player: Player, position: Vect2) = ???
  override def attack(player: Player, source: Vect2, target: Vect2) = ???

  def nextTeamTurn: Evented[TurnBasedGame] =
    game.teamTurnFinished(currentTeam).flatMap { g =>
      val newActedTeams = actedTeams :+ currentTeam
      if (readyTeams.isEmpty) g.gameTurnFinished.flatMap(
        TurnBasedGame.newGameTurn(_, newActedTeams)
      )
      else {
        val newCurrentTeam = readyTeams.head
        g.teamTurnStarted(newCurrentTeam).map(
          TurnBasedGame(_, newCurrentTeam, readyTeams.tail, newActedTeams)
        )
      }
    }
}
