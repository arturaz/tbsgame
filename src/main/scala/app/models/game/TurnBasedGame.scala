package app.models.game

import app.models.game.events.Evented
import app.models.world.{World, Vect2, Warpable, WarpableCompanion}
import app.models.{Player, Team}
import implicits._

object TurnBasedGame {
  type Result = Game.ResultT[TurnBasedGame]
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
  def canAct(player: Player) = player.team == currentTeam

  def playerDo(player: Player)(f: Player => Game.Result): TurnBasedGame.Result =
    if (canAct(player)) f(player).right.map(_.map(game => copy(game = game)))
    else s"$player cannot act, because current team is $currentTeam".left

  override def warp(
    player: Player, position: Vect2, warpable: WarpableCompanion[_ <: Warpable]
  ) = playerDo(player)(game.warp(_, position, warpable))

  override def move(player: Player, from: Vect2, to: Vect2) =
    playerDo(player)(game.move(_, from, to))

  override def special(player: Player, position: Vect2) =
    playerDo(player)(game.special(_, position))

  override def attack(player: Player, source: Vect2, target: Vect2) =
    playerDo(player)(game.attack(_, source, target))

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
