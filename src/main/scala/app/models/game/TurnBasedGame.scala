package app.models.game

import app.models.game.events.Evented
import app.models.world._
import app.models.{Human, Team}
import implicits._

object TurnBasedGame {
  type Result = Game.ResultT[TurnBasedGame]
  type StartedGame = Either[String, Evented[TurnBasedGame]]

  def apply(world: World, startingResources: Int): StartedGame =
    Game(world, startingResources).right.flatMap(apply)

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
  def canAct(human: Human) = human.team == currentTeam

  def update(f: => Game.Result): TurnBasedGame.Result =
    f.right.map(_.map(updated))
  def updated(game: Game) = copy(game = game)

  def humanDo(human: Human)(f: Human => Game.Result): TurnBasedGame.Result =
    if (canAct(human)) update(f(human))
    else s"$human cannot act, because current team is $currentTeam".left

  override def join(human: Human, startingResources: Int) =
    update(game.join(human, startingResources))

  override def leave(human: Human) =
    update(game.leave(human))

  override def warp(
    human: Human, position: Vect2, warpable: WarpableCompanion[_ <: Warpable]
  ) = humanDo(human)(game.warp(_, position, warpable))

  override def move(human: Human, id: WObject.Id, to: Vect2) =
    humanDo(human)(game.move(_, id, to))

  override def special(human: Human, id: WObject.Id) =
    humanDo(human)(game.special(_, id))

  override def attack(human: Human, id: WObject.Id, targetId: WObject.Id) =
    humanDo(human)(game.attack(_, id, targetId))

  override def consumeActions(human: Human) = humanDo(human)(game.consumeActions)

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

  def currentTeamActionsLeft = game.actionsLeftFor(currentTeam)
  def currentTeamFinished = currentTeamActionsLeft == 0
}
