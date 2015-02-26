package app.models.game

import monocle.Lenser

object GamePlayerState {
  private[this] val lenser = Lenser[GamePlayerState]

  val actions = lenser(_.actions)

  sealed trait State {
    def canAct: Boolean
  }
  case object WaitingForTurnEnd extends State {
    override val canAct = true
  }
  case object TurnEnded extends State {
    override val canAct = false
  }
  case object Conceded extends State {
    override val canAct = false
  }

  val conceded = GamePlayerState(Actions(0), GamePlayerState.Conceded)
}

case class GamePlayerState(actions: Actions, activity: GamePlayerState.State) {
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
