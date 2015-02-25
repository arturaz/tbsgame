package app.models.game

import akka.event.LoggingAdapter
import app.models.game.Game.States
import app.models.game.events.Evented
import app.models.game.world.WObject.Id
import app.models.game.world._
import implicits._
import utils.data.NonEmptyVector

import scalaz.{\/-, -\/, \/}

object TurnBasedGame {
  type Result = Game.ResultT[TurnBasedGame]
  type StartedGame = Either[String, Evented[TurnBasedGame]]

  def apply(
    world: World, starting: Set[Game.StartingPlayer],
    objectives: Game.ObjectivesMap
  )(implicit log: LoggingAdapter): StartedGame = {
    val game = Game(world, starting, objectives)
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
    val turnBasedGame = teamTurnStartedGame.map { game =>
      apply(
        endTurnForNonCurrentTeamPlayers(game, startingTeam),
        startingTeam, teams.tail, Vector.empty
      )
    }
    turnBasedGame
  }

  private[this] def endTurnForNonCurrentTeamPlayers(game: Game, currentTeam: Team) = {
    game.copy(states = game.states.map {
      case orig @ (player, state) if player.team === currentTeam => orig
      case (player, state) => player -> state.onTurnEnd
    })
  }
}

case class TurnBasedGame private (
  game: Game,
  currentTeam: Team, readyTeams: Vector[Team], actedTeams: Vector[Team]
) extends GameLike[TurnBasedGame] {
  def canAct(human: Human) = human.team === currentTeam

  def update(f: => Game.Result): TurnBasedGame.Result =
    f.right.map(_.map(updated))
  def updated(game: Game) = copy(game = game)

  def humanDo(human: Human)(f: Human => Game.Result): TurnBasedGame.Result =
    if (canAct(human)) update(f(human))
    else s"$human cannot act, because current team is $currentTeam".left

  override def isJoined(human: Human)
  (implicit log: LoggingAdapter) = game.isJoined(human)

  override def join(human: Human, startingResources: Resources)
  (implicit log: LoggingAdapter) =
    update(game.join(human, startingResources))

  override def leave(human: Human)(implicit log: LoggingAdapter) =
    update(game.leave(human))

  override def warp(
    human: Human, position: Vect2,
    warpable: WarpableCompanion.Some
  )(implicit log: LoggingAdapter) = humanDo(human)(game.warp(_, position, warpable))

  override def move(human: Human, id: WObject.Id, path: NonEmptyVector[Vect2])
  (implicit log: LoggingAdapter) =
    humanDo(human)(game.move(_, id, path))

  override def special(human: Human, id: WObject.Id)(implicit log: LoggingAdapter) =
    humanDo(human)(game.special(_, id))

  override def attack(human: Human, id: WObject.Id, targetId: WObject.Id)
  (implicit log: LoggingAdapter) =
    humanDo(human)(game.attack(_, id, targetId))

  override def moveAttack(human: Human, id: Id, path: NonEmptyVector[Vect2], targetId: Id)
  (implicit log: LoggingAdapter) =
    humanDo(human)(game.moveAttack(_, id, path, targetId))

  override def endTurn(human: Human)(implicit log: LoggingAdapter) =
    humanDo(human)(game.endTurn)

  override def concede(human: Human)(implicit log: LoggingAdapter) =
    update(game.concede(human))

  def nextTeamTurn(implicit log: LoggingAdapter): Evented[Winner \/ TurnBasedGame] = {
    log.debug("current team finishing: {}", currentTeam)
    game.teamTurnFinished(currentTeam).flatMap {
      case left @ -\/(winner) =>
        log.debug("we have a winner: {}", winner)
        Evented(left)
      case \/-(g) =>
        val newActedTeams = actedTeams :+ currentTeam
        log.debug("new acted teams: {}", newActedTeams)
        if (readyTeams.isEmpty) {
          log.debug("no more ready teams!")
          g.gameTurnFinished.flatMap(TurnBasedGame.newGameTurn(_, newActedTeams)).
            map(_.rightZ)
        }
        else {
          val newCurrentTeam = readyTeams.head
          log.debug("new current team: {}", newCurrentTeam)
          g.teamTurnStarted(newCurrentTeam).map(
            TurnBasedGame(_, newCurrentTeam, readyTeams.tail, newActedTeams).rightZ
          )
        }
    }
  }

  def currentTeamFinished = game.allPlayersTurnEnded(currentTeam)
}
