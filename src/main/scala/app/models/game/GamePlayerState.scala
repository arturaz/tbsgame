package app.models.game

import app.models.game.world.WarpableCompanion
import monocle.Lenser

object GamePlayerState {
  type CanWarp = Set[WarpableCompanion.Some]

  private[this] val lenser = Lenser[GamePlayerState]

  val actions = lenser(_.actions)

  sealed trait Activity {
    def canAct: Boolean
  }
  case object WaitingForTurnEnd extends Activity {
    override val canAct = true
  }
  case object TurnEnded extends Activity {
    override val canAct = false
  }
  case object Conceded extends Activity {
    override val canAct = false
  }

  val conceded = GamePlayerState(Actions(0), GamePlayerState.Conceded, Set.empty)
}

case class GamePlayerState(
  actions: Actions, activity: GamePlayerState.Activity, canWarp: GamePlayerState.CanWarp
) {
  def onTurnStart(player: Player, newActions: Actions) = activity match {
    case GamePlayerState.Conceded =>
      GamePlayerState.conceded
    case GamePlayerState.WaitingForTurnEnd | GamePlayerState.TurnEnded
    if player.isBot =>
      copy(actions = newActions, activity = GamePlayerState.TurnEnded)
    case GamePlayerState.WaitingForTurnEnd | GamePlayerState.TurnEnded =>
      copy(actions = newActions, activity = GamePlayerState.WaitingForTurnEnd)
  }

  def onTurnEnd = activity match {
    case GamePlayerState.Conceded =>
      GamePlayerState.conceded
    case GamePlayerState.WaitingForTurnEnd | GamePlayerState.TurnEnded =>
      copy(activity = GamePlayerState.TurnEnded)
  }
}
