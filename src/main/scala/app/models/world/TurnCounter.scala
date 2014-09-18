package app.models.world

trait TurnCounter extends WObject {
  type Self <: TurnCounter
  val turns: Int

  override def nextTurn = withTurns(super.nextTurn, turns + 1)

  protected def withTurns(self: Self, turns: Int): Self
}
