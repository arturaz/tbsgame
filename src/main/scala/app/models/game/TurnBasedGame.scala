package app.models.game

import akka.event.LoggingAdapter
import app.models.game.Game.States
import app.models.game.GamePlayerState.WaitingForTurnEnd
import app.models.game.TurnBasedGame.{WithCurrentTime, TurnTimersMap}
import app.models.game.events.Evented
import app.models.game.world.WObject.Id
import app.models.game.world._
import implicits._
import org.joda.time.DateTime
import utils.data.NonEmptyVector

import scalaz.{\/-, -\/, \/}

object TurnBasedGame {
  type Result = Game.ResultT[TurnBasedGame]
  type StartedGame = Either[String, Evented[TurnBasedGame]]
  type TurnTimersMap = Map[Human, TurnTimer]

  case class WithCurrentTime[+A](value: A, currentTime: DateTime) {
    def map[B](f: A => B) = copy(value = f(value))
    def apply[B](f: (A, DateTime) => B) = f(value, currentTime)
  }

  def apply(
    world: World, starting: Set[Game.StartingPlayer],
    objectives: Game.ObjectivesMap,
    turnTimerSettings: Option[WithCurrentTime[TurnTimers.Settings]]
  )(implicit log: LoggingAdapter): StartedGame = {
    val game = Game(world, starting, objectives)
    game.right.flatMap(apply(_, turnTimerSettings))
  }

  def apply(game: Game, turnTimerSettings: Option[WithCurrentTime[TurnTimers.Settings]])
  (implicit log: LoggingAdapter): StartedGame = {
    val teams = game.world.teams.toVector
    val turnTimers = turnTimerSettings.map(_.map(TurnTimers(game.world.humans, _)))
    if (teams.isEmpty) s"No teams found in $game!".left
    else newGameTurn(game, teams, turnTimers).right
  }

  private def newGameTurn(
    game: Game, teams: Vector[Team], turnTimers: Option[WithCurrentTime[TurnTimers]]
  )(implicit log: LoggingAdapter): Evented[TurnBasedGame] = {
    val startingTeam = teams.head
    val gameTurnStartedGame = game.gameTurnStarted
    val turnBasedGame = for {
      game <- gameTurnStartedGame.flatMap(_.teamTurnStarted(startingTeam))
      timers <- turnTimers.map(_.apply(_.teamTurnStarted(startingTeam, _))).extract
    } yield apply(
      endTurnForNonCurrentTeamPlayers(game, startingTeam),
      startingTeam, teams.tail, Vector.empty, timers
    )
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
  currentTeam: Team, readyTeams: Vector[Team], actedTeams: Vector[Team],
  turnTimers: Option[TurnTimers]
) {
  def canAct(human: Human) = human.team === currentTeam

  def update(f: => Game.Result): TurnBasedGame.Result =
    f.right.map(_.map(updated))
  def updated(game: Game) = copy(game = game)

  def humanDo(human: Human)(f: Human => Game.Result): TurnBasedGame.Result =
    humanDoTBG(human)(human => update(f(human)))

  def humanDoTBG(human: Human)(f: Human => TurnBasedGame.Result): TurnBasedGame.Result =
    if (canAct(human)) f(human)
    else s"$human cannot act, because current team is $currentTeam".left

  def isJoined(human: Human)
  (implicit log: LoggingAdapter) = game.isJoined(human)

  def join(human: Human, startingResources: Resources)
  (implicit log: LoggingAdapter) =
    update(game.join(human, startingResources))

  def leave(human: Human)(implicit log: LoggingAdapter) =
    update(game.leave(human))

  def warp(
    human: Human, position: Vect2,
    warpable: WarpableCompanion.Some
  )(implicit log: LoggingAdapter) = humanDo(human)(game.warp(_, position, warpable))

  def move(human: Human, id: WObject.Id, path: NonEmptyVector[Vect2])
  (implicit log: LoggingAdapter) =
    humanDo(human)(game.move(_, id, path))

  def special(human: Human, id: WObject.Id)(implicit log: LoggingAdapter) =
    humanDo(human)(game.special(_, id))

  def attack(human: Human, id: WObject.Id, targetId: WObject.Id)
  (implicit log: LoggingAdapter) =
    humanDo(human)(game.attack(_, id, targetId))

  def moveAttack(human: Human, id: Id, path: NonEmptyVector[Vect2], targetId: Id)
  (implicit log: LoggingAdapter) =
    humanDo(human)(game.moveAttack(_, id, path, targetId))

  def endTurn(human: Human, currentTime: DateTime)(implicit log: LoggingAdapter) =
    humanDoTBG(human) { human =>
      game.endTurn(human).right.map { evtGame =>
        evtGame.map { game =>
          copy(game = game, turnTimers = turnTimers.map(_.endTurn(human, currentTime)))
        }
      }
    }

  def concede(human: Human)(implicit log: LoggingAdapter) =
    update(game.concede(human))

  def nextTeamTurn(currentTime: DateTime)
  (implicit log: LoggingAdapter): Evented[Winner \/ TurnBasedGame] = {
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
          g.gameTurnFinished.flatMap(TurnBasedGame.newGameTurn(
            _, newActedTeams, turnTimers.map(WithCurrentTime(_, currentTime))
          )).map(_.rightZ)
        }
        else {
          val newCurrentTeam = readyTeams.head
          log.debug("new current team: {}", newCurrentTeam)
          g.teamTurnStarted(newCurrentTeam).flatMap { game =>
            turnTimers.map(_.teamTurnStarted(newCurrentTeam, currentTime))
              .extract.map(TurnBasedGame(
                game, newCurrentTeam, readyTeams.tail, newActedTeams, _
              ).rightZ)
          }
        }
    }
  }

  def currentTeamFinished = game.allPlayersTurnEnded(currentTeam)

  def checkTurnTimes(currentTime: DateTime)(implicit log0: LoggingAdapter)
  : Evented[TurnBasedGame] = {
    lazy val log = log0.prefixed("checkTurnTimes")
    turnTimers match {
      case None => Evented(this)
      case Some(timers) =>
        timers.map.foldLeft(Evented(this)) {
          case (evtTbg, (human, TurnTimer(_, Some(timeframe))))
          if human.team === currentTeam && timeframe.timeUsedUp(currentTime)
          && evtTbg.value.game.states.get(human).fold2(
            {
              log.error(
                "cannot find {} in game player states {}", human, evtTbg.value.game.states
              )
              false
            },
            _.activity === WaitingForTurnEnd
          ) =>
            evtTbg.flatMap(tbg => {
              tbg.endTurn(human, currentTime).fold(
                err => {
                  log.error("TurnBasedGame#endTurn failed for {} with {}", human, err)
                  Evented(tbg)
                },
                identity
              )
            })
          case (evtTbg, _) => evtTbg
        }
    }
  }
}
