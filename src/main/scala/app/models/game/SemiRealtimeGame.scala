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

import scalaz.{\/, \/-, -\/}

object SemiRealtimeGame extends GameActorGameStarter[SemiRealtimeGame] {
  private def gameTurnStarted(game: Game)(implicit log: LoggingAdapter) = {
    game.gameTurnStarted.flatMap { game =>
      game.world.teams.foldLeft(Evented(game)) { case (evtWinnerOrGame, team) =>
        evtWinnerOrGame.flatMap(_.teamTurnStarted(team)) match { case Evented(g, events) =>
          Evented(g, events.filter {
            case e: TurnStartedEvt => false
            case _ => true
          })
        }
      }
    }
  }

  override def newGameTurn(
    game: Game, teams: Vector[Team], turnTimers: Option[WithCurrentTime[TurnTimers]]
  )(implicit log: LoggingAdapter) = {
    gameTurnStarted(game).flatMap { game =>
      val spinner = TurnSpinner(playerOrder(game))
//      val evtOptTurnTimers = turnTimers.map(_ { (tt, currentTime) =>
//        tt.turnStarted(spinner.current, currentTime)
//      }).extract
//      evtOptTurnTimers.flatMap { optTurnTimers =>
        Evented(
          SemiRealtimeGame(game, spinner, None /*TODO:optTurnTimers*/),
          TurnStartedEvt(spinner.current.team)
        )
//      }
    }
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

  type Result = Game.ResultT[SemiRealtimeGame]
}

case class TurnSpinner[A](current: A, ready: Vector[A], acted: Vector[A]) {
  def next = {
    val newActed = acted :+ current
    if (ready.isEmpty) TurnSpinner(newActed)
    else TurnSpinner(ready.head, ready.tail, newActed)
  }
}
object TurnSpinner {
  def apply[A](v: Vector[A]): TurnSpinner[A] = apply(v.head, v.tail, Vector.empty)
}

case class SemiRealtimeGame(
  game: Game, turnSpinner: TurnSpinner[Human], turnTimers: Option[TurnTimers]
) extends GameActorGame with GameActorGame.CheckTurnTime[SemiRealtimeGame]
{
  override def update(game: Game, turnTimers: Option[TurnTimers]) ={
    def rec(curSpin: TurnSpinner[Human]): TurnSpinner[Human] = {
      if (game.turnEnded(curSpin.current)) rec(curSpin.next)
      else curSpin
    }

    val newSpinner =
      if (game.allPlayersTurnEnded) turnSpinner.next
      else rec(turnSpinner.next)
    val newGame = copy(game = game, turnSpinner = newSpinner, turnTimers = turnTimers)
    val evtGame = Evented(
      newGame,
      Vector(
        TurnEndedEvt(turnSpinner.current.team),
        TurnStartedEvt(newSpinner.current.team)
      )
    )
    evtGame
  }

  def canAct(human: Human) = human === turnSpinner.current

  def humanDo(human: Human)(f: Human => Game.Result) =
    humanDoTBG(human) { human =>
      updateCTT(human, DateTime.now())(f(human))
    }

  def humanDoTBG(human: Human)(f: Human => SemiRealtimeGame.Result)
  : SemiRealtimeGame.Result =
    if (canAct(human)) f(human)
    else s"$human cannot act, because current is ${turnSpinner.current}".left

  override def warp(human: Human, position: Vect2, warpable: Some)
  (implicit log: LoggingAdapter) =
    humanDo(human)(game.warp(_, position, warpable))

  override def move(human: Human, id: Id, path: NonEmptyVector[Vect2])
  (implicit log: LoggingAdapter) = humanDo(human)(game.move(_, id, path))

  override def special(human: Human, id: Id)(implicit log: LoggingAdapter) =
    humanDo(human)(game.special(_, id))

  override def attack(human: Human, id: Id, targetId: Id)(implicit log: LoggingAdapter) =
    humanDo(human)(game.attack(_, id, targetId))

  override def moveAttack(human: Human, id: Id, path: NonEmptyVector[Vect2], targetId: Id)
    (implicit log: LoggingAdapter) = humanDo(human)(game.moveAttack(_, id, path, targetId))

  override def endTurn(human: Human, currentTime: DateTime)
    (implicit log: LoggingAdapter) = humanDo(human)(game.endTurn)

  override def concede(human: Human)(implicit log: LoggingAdapter) =
    humanDo(human)(game.concede)

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

    val unfiltered = teamTurnsStarted.map(_.map(game => copy(game = game)))
    val filtered = Evented(
      unfiltered.value,
      unfiltered.events.filter {
        case _: TurnStartedEvt | _: TurnEndedEvt => false
        case _ => true
      }
    )
    filtered
  }

  override def currentTeam(human: Human) = turnSpinner.current.team

  override def isJoined(human: Human)(implicit log: LoggingAdapter) = game.isJoined(human)
}
