package app.models.game

import app.models.game.world.WarpableCompanion
import monocle.Lenser
import implicits._
import scalaz._, Scalaz._

object GamePlayerState {
  type CanWarp = Vector[WarpableCompanion.Some]

  private[this] val lenser = Lenser[GamePlayerState]

  val actions = lenser(_.actions)

  sealed trait Activity {
    def canAct: Boolean
  }
  case object Active extends Activity {
    override val canAct = true
  }
  case object WaitingForNextRound extends Activity {
    override val canAct = false
  }
  case object Conceded extends Activity {
    override val canAct = false
  }

  val conceded = GamePlayerState(Actions(0), GamePlayerState.Conceded, Vector.empty)
}

case class GamePlayerState(
  actions: Actions, activity: GamePlayerState.Activity, canWarp: GamePlayerState.CanWarp
) {
  def onRoundStart(player: Player, newActions: Actions) = activity match {
    case GamePlayerState.Conceded =>
      GamePlayerState.conceded
    case GamePlayerState.Active | GamePlayerState.WaitingForNextRound
    if player.isBot =>
      copy(actions = newActions, activity = GamePlayerState.WaitingForNextRound)
    case GamePlayerState.Active | GamePlayerState.WaitingForNextRound =>
      copy(actions = newActions, activity = GamePlayerState.Active)
  }

  def waitForNextRound(toggled: Boolean) = activity match {
    case GamePlayerState.Conceded =>
      GamePlayerState.conceded
    case GamePlayerState.Active | GamePlayerState.WaitingForNextRound =>
      copy(
        activity =
          if (toggled) GamePlayerState.WaitingForNextRound else GamePlayerState.Active
      )
  }

  def toggleWaitForNextRound = waitForNextRound(activity.canAct)
}
