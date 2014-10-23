package utils.data

case class NonEmptyVector[+A] private (v: Vector[A]) extends AnyVal

object NonEmptyVector {
  def create[A](v: Vector[A]): Option[NonEmptyVector[A]] =
    if (v.isEmpty) None else Some(NonEmptyVector(v))
}
