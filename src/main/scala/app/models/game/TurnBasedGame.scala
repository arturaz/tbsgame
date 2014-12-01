package app.models.game

import akka.event.LoggingAdapter
import app.models.game.events.Evented
import app.models.game.world._
import implicits._
import utils.data.NonEmptyVector

object TurnBasedGame {
  type Result = Game.ResultT[TurnBasedGame]
  type StartedGame = Either[String, Evented[TurnBasedGame]]

  def apply(
    world: World, startingHuman: Human, startingResources: Resources
  )(implicit log: LoggingAdapter): StartedGame = {
    val game = Game(world, startingHuman, startingResources)
    game.right.flatMap(apply)
  }

  def apply(game: Game)(implicit log: LoggingAdapter): StartedGame = {
    val teams = game.world.teams.toVector
    if (teams.isEmpty) s"No teams found in $game!".left
    else newGameTurn(game, teams).right
  }

  private def newGameTurn(game: Game, teams: Vector[Team])(implicit log: LoggingAdapter)
  : Evented[TurnBasedGame] = {
    val startingTeam = teams.head
    val gameTurnStartedGame = game.gameTurnStarted
    val teamTurnStartedGame = gameTurnStartedGame.flatMap(_.teamTurnStarted(startingTeam))
    val turnBasedGame = teamTurnStartedGame.map {
      apply(_, startingTeam, teams.tail, Vector.empty)
    }
    turnBasedGame
  }
}

case class TurnBasedGame private (
  game: Game, currentTeam: Team, readyTeams: Vector[Team], actedTeams: Vector[Team]
) extends GameLike[TurnBasedGame] {
  def canAct(human: Human) = human.team === currentTeam

  def update(f: => Game.Result): TurnBasedGame.Result =
    f.right.map(_.map(updated))
  def updated(game: Game) = copy(game = game)

  def humanDo(human: Human)(f: Human => Game.Result): TurnBasedGame.Result =
    if (canAct(human)) update(f(human))
    else s"$human cannot act, because current team is $currentTeam".left

  override def isJoined(human: Human) = game.isJoined(human)

  override def join(human: Human, startingResources: Resources) =
    update(game.join(human, startingResources))

  override def leave(human: Human) =
    update(game.leave(human))

  override def warp(
    human: Human, position: Vect2, warpable: WarpableCompanion[_ <: Warpable]
  ) = humanDo(human)(game.warp(_, position, warpable))

  override def move(human: Human, id: WObject.Id, path: NonEmptyVector[Vect2]) =
    humanDo(human)(game.move(_, id, path))

  override def special(human: Human, id: WObject.Id) =
    humanDo(human)(game.special(_, id))

  override def attack(human: Human, id: WObject.Id, targetId: WObject.Id) =
    humanDo(human)(game.attack(_, id, targetId))

  override def endTurn(human: Human) = humanDo(human)(game.endTurn)

  def nextTeamTurn(implicit log: LoggingAdapter): Evented[TurnBasedGame] = {
    log.debug("current team finishing: {}", currentTeam)
    game.teamTurnFinished(currentTeam).flatMap { g =>
      val newActedTeams = actedTeams :+ currentTeam
      log.debug("new acted teams: {}", newActedTeams)
      if (readyTeams.isEmpty) {
        log.debug("no more ready teams!")
        g.gameTurnFinished.flatMap(
          TurnBasedGame.newGameTurn(_, newActedTeams)
        )
      }
      else {
        val newCurrentTeam = readyTeams.head
        log.debug("new current team: {}", newCurrentTeam)
        g.teamTurnStarted(newCurrentTeam).map(
          TurnBasedGame(_, newCurrentTeam, readyTeams.tail, newActedTeams)
        )
      }
    }
  }

  def currentTeamFinished = game.allPlayersTurnEnded(currentTeam)
}
