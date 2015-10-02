package app.models.game

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