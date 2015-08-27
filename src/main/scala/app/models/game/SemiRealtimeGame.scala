package app.models.game

import akka.event.LoggingAdapter
import app.actors.game.{GameActorGameStarter, GameActorGame}
import app.models.game.events.{TurnStartedSemiRealtimeEvt, TurnStartedEvt, Evented}
import app.models.game.world.Vect2
import app.models.game.world.WObject.Id
import app.models.game.world.WarpableCompanion.Some
import org.joda.time.DateTime
import utils.data.NonEmptyVector
import implicits._

import scalaz.{\/, \/-, -\/}

object SemiRealtimeGame extends GameActorGameStarter[SemiRealtimeGame] {
  private def gameTurnStarted(game: Game)(implicit log: LoggingAdapter) = {
    game.gameTurnStarted.flatMap { game =>
      game.world.teams.foldLeft(Evented(game)) { case (evtWinnerOrGame, team) =>
        evtWinnerOrGame.flatMap(_.teamTurnStarted(team)) match { case Evented(g, events) =>
          Evented(g, events.map {
            case e: TurnStartedEvt => TurnStartedSemiRealtimeEvt(e.team)
            case e => e
          })
        }
      }
    }
  }

  override def newGameTurn(
    game: Game, teams: Vector[Team], turnTimers: Option[WithCurrentTime[TurnTimers]]
  )(implicit log: LoggingAdapter) = {
    gameTurnStarted(game).map { game =>

      turnTimers.map(_ { (tt, currentTime) => tt.turnStarted(human, currentTime) })
      SemiRealtimeGame(game, )
    }
  }
}

// TODO: add turn timers
case class SemiRealtimeGame(
  game: Game, currentlyControlling: Human, turnTimers: Option[TurnTimers]
) extends GameActorGame with GameActorGame.CheckTurnTime[SemiRealtimeGame]
{
  def update(f: Game.Result) = f.right.map(_.map(updated))
  def updated(game: Game) = copy(game = game)

//  override def update(game: Game, turnTimers: Option[TurnTimers]) =
//    copy(game = game, turnTimers = turnTimers)

  override def warp(human: Human, position: Vect2, warpable: Some)
  (implicit log: LoggingAdapter) = game.warp(human, position, warpable) |> update

  override def move(human: Human, id: Id, path: NonEmptyVector[Vect2])
  (implicit log: LoggingAdapter) = game.move(human, id, path) |> update

  override def special(human: Human, id: Id)(implicit log: LoggingAdapter) =
    game.special(human, id) |> update

  override def attack(human: Human, id: Id, targetId: Id)(implicit log: LoggingAdapter) =
    game.attack(human, id, targetId) |> update

  override def moveAttack(human: Human, id: Id, path: NonEmptyVector[Vect2], targetId: Id)
    (implicit log: LoggingAdapter) = game.moveAttack(human, id, path, targetId) |> update

  override def endTurn(human: Human, currentTime: DateTime)
    (implicit log: LoggingAdapter) = game.endTurn(human) |> update
  //endTurnCTT(human, currentTime)

  override def concede(human: Human)(implicit log: LoggingAdapter) =
    game.concede(human) |> update

  override def shouldAdvanceTurn = game.world.teams.forall(game.allPlayersTurnEnded)

  override def advanceTurn(currentTime: DateTime)(implicit log: LoggingAdapter) = {
    def onGame[A, B](evtWinnerOrA: Evented[Winner \/ A])(f: A => Evented[Winner \/ B]) =
      evtWinnerOrA.flatMap {
        case left @ -\/(winner) => Evented(left)
        case \/-(a) => f(a)
      }

    val teamTurnsFinished = game.world.teams.foldLeft(Evented(game.rightZ[Winner])) {
      case (evtWinnerOrGame, team) => onGame(evtWinnerOrGame)(_.teamTurnFinished(team))
    }

    val gameTurnFinished = onGame(teamTurnsFinished)(_.gameTurnFinished.map(_.rightZ))

    val teamTurnsStarted =
      onGame(gameTurnFinished)(SemiRealtimeGame.gameTurnStarted(_).map(_.rightZ))

    teamTurnsStarted.map(_.map(SemiRealtimeGame.apply))
  }

  override def currentTeam(human: Human) = human.team

  override def isJoined(human: Human)(implicit log: LoggingAdapter) = game.isJoined(human)
}
