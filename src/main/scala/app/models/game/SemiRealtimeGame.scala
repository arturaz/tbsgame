package app.models.game

import akka.event.LoggingAdapter
import app.actors.game.{GameActorGameStarter, GameActorGame}
import app.models.game.events._
import app.models.game.world.Vect2
import app.models.game.world.WObject.Id
import app.models.game.world.WarpableCompanion.Some
import org.joda.time.DateTime
import utils.data.NonEmptyVector
import implicits._
import scala.annotation.tailrec
import scalaz._, Scalaz._

object SemiRealtimeGame extends GameActorGameStarter[SemiRealtimeGame] {
  override def startNewGame(
    game: Game, turnTimers: Option[WithCurrentTime[TurnTimers]]
  )(implicit log: LoggingAdapter) = {
    if (game.world.teams.size < 2) {
      s"Need at least 2 teams, but had ${game.world.teams}!".left
    }
    else game.roundStarted.map { game =>
      val spinner = TurnSpinner(playerOrder(game))
      val optTurnTimers = turnTimers.map(_ { (tt, currentTime) =>
        tt.turnStarted(spinner.current, currentTime)
      })
      SemiRealtimeGame(game, spinner, optTurnTimers)
    }.right
  }

  def playerOrder(game: Game): Vector[Human] = {
    val humans = game.world.humans.toVector.groupBy(_.team)
    val teams = game.world.teams.filter(humans.contains).toVector

    val maxPlayers = humans.values.map(_.size).max
    val order = for {
      idx <- 0 until maxPlayers
      team <- teams.map(humans)
      human <- team.lift(idx)
    } yield human
    order.toVector
  }

  type Result = Game.ResultT[Winner \/ SemiRealtimeGame]
}

case class SemiRealtimeGame(
  game: Game, turnSpinner: TurnSpinner[Human], turnTimers: Option[TurnTimers]
) extends GameActorGame
{
  def updateF(result: Game.ResultOrWinner)(
    f: Game => Evented[SemiRealtimeGame]
  )(implicit log: LoggingAdapter)
  : SemiRealtimeGame.Result =
    result.map { evtGame =>
      evtGame.flatMap { winnerOrGame =>
        winnerOrGame.map(f).extract
      }
    }

  def update(result: Game.ResultOrWinner)(implicit log: LoggingAdapter)
  : SemiRealtimeGame.Result = updateF(result)(game => Evented(updated(game)))

  def updated(game: Game) = copy(game = game)

  def update(result: Game.ResultOrWinner, now: DateTime)(implicit log: LoggingAdapter)
  : SemiRealtimeGame.Result = updateF(result)(update(_, now))

  def update(game: Game, now: DateTime)(implicit log: LoggingAdapter)
  : Evented[SemiRealtimeGame] = {
    /* Find new player which can act, doing auto specials for the skipped players in the
       meantime. */
    @tailrec def rec(current: Evented[(Game, TurnSpinner[Human])])
    : Evented[(Game, TurnSpinner[Human])] = {
      val (game, curSpin) = current.value
      if (game.allPlayersFinished) {
        val newEvtGame = current.flatMap { _ =>
          game.roundEnded.flatMap(_.roundStarted).map((_, curSpin))
        }
        rec(newEvtGame)
      }
      else {
        val player = curSpin.current
        val curState = game.states(player)
        if (!curState.activity.canAct) {
          val newEvtGame = current.flatMap { _ =>
            if (curState.actions.isNotZero) Game.doAutoSpecialIgnoreErrors(game, player)
            else Evented(game)
          }.map((_, curSpin.next))
          rec(newEvtGame)
        }
        else current
      }
    }

    val newEvtGameWithSpinner = rec(Evented((game, turnSpinner.next)))
    newEvtGameWithSpinner.flatMap { case (game, newSpinner) =>
      val newTT = turnTimers.map {
        _.endTurn(turnSpinner.current, now).turnStarted(newSpinner.current, now)
      }
      val newGame = copy(game = game, turnSpinner = newSpinner, turnTimers = newTT)
      Evented(
        newGame,
        Vector(newGame.currentTurnStartedEvt)
      )
    }
  }

  def canAct(human: Human) = human === turnSpinner.current

  def humanDo(human: Human, now: DateTime)(f: Human => Game.ResultOrWinner)(
    implicit log: LoggingAdapter
  ) = {
    if (canAct(human)) update(f(human), now)
    else s"$human cannot act, because current is ${turnSpinner.current}".left
  }

  override def warp(human: Human, position: Vect2, warpable: Some, now: DateTime)
  (implicit log: LoggingAdapter) =
    humanDo(human, now)(game.warp(_, position, warpable))

  override def move(human: Human, id: Id, path: NonEmptyVector[Vect2], now: DateTime)
  (implicit log: LoggingAdapter) = humanDo(human, now)(game.move(_, id, path))

  override def special(
    human: Human, id: Id, now: DateTime
  )(implicit log: LoggingAdapter) =
    humanDo(human, now)(game.special(_, id))

  override def attack(
    human: Human, id: Id, targetId: Id, now: DateTime
  )(implicit log: LoggingAdapter) =
    humanDo(human, now)(game.attack(_, id, targetId))

  override def moveAttack(
    human: Human, id: Id, path: NonEmptyVector[Vect2], targetId: Id, now: DateTime
  )(implicit log: LoggingAdapter) =
    humanDo(human, now)(game.moveAttack(_, id, path, targetId))

  def turnTimeEnded(human: Human, now: DateTime)
    (implicit log: LoggingAdapter) = update(game.turnTimeEnded(human), now)

  override def toggleWaitingForRoundEnd(
    human: Human, now: DateTime
  )(implicit log: LoggingAdapter) = {
    val state = game.states(human)
    val result =
      if (turnSpinner.current === human) update(
        if (state.actions.isNotZero && state.activity.canAct)
          Game.doAutoSpecialIgnoreErrors(game, human)
            .map(_.toggleWaitForNextRound(human)).extractFlatten
        else
          game.toggleWaitForNextRound(human),
        now
      )
      else
        update(game.toggleWaitForNextRound(human))
    result
  }

  override def concede(human: Human, now: DateTime)(implicit log: LoggingAdapter) =
    update(game.concede(human), now)

  override def currentPlayer = turnSpinner.current

  override def isJoined(human: Human)(implicit log: LoggingAdapter) = game.isJoined(human)

  override def currentTurnTimeframe =
    turnTimers.flatMap(_.map(turnSpinner.current).currentTurn)

  override def checkTurnTimes(currentTime: DateTime)(implicit log0: LoggingAdapter) = {
    lazy val log = log0.prefixed("checkTurnTimes")
    turnTimers match {
      case None => Evented(this.right)
      case Some(timers) =>
        timers.map.foldLeft(Evented(this.right[Winner])) {
          case (
            Evented(\/-(srGame), prevEvents),
            (human, TurnTimer(_, Some(timeframe)))
          ) if human === turnSpinner.current && timeframe.timeUsedUp(currentTime)
            && srGame.game.states.get(human).fold2(
              {
                log.error(
                  "cannot find {} in game player states {}",
                  human, srGame.game.states
                )
                false
              },
              _.activity === GamePlayerState.Active
            )
          =>
            prevEvents ++: srGame.turnTimeEnded(human, currentTime).fold(
              err => {
                log.error("TurnBasedGame#turnTimeEnded failed for {} with {}", human, err)
                Evented(srGame.right)
              },
              identity
            )
          case (evtTbg, _) => evtTbg
        }
    }
  }
}
