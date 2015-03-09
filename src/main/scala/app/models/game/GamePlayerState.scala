package app.models.game

import monocle.Lenser

object GamePlayerState {
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

  val conceded = GamePlayerState(Actions(0), GamePlayerState.Conceded)
}

case class GamePlayerState(actions: Actions, activity: GamePlayerState.Activity) {
  def onTurnStart(player: Player, actions: Actions) = activity match {
    case GamePlayerState.Conceded =>
      GamePlayerState.conceded
    case GamePlayerState.WaitingForTurnEnd | GamePlayerState.TurnEnded
    if player.isBot =>
      GamePlayerState(actions, GamePlayerState.TurnEnded)
    case GamePlayerState.WaitingForTurnEnd | GamePlayerState.TurnEnded =>
      GamePlayerState(actions, GamePlayerState.WaitingForTurnEnd)
  }

  def onTurnEnd = activity match {
    case GamePlayerState.Conceded =>
      GamePlayerState.conceded
    case GamePlayerState.WaitingForTurnEnd | GamePlayerState.TurnEnded =>
      copy(activity = GamePlayerState.TurnEnded)
  }
}
