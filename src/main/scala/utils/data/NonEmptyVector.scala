package utils.data

import implicits._

import scalaz.{Plus, Monad}

case class NonEmptyVector[+A] private (v: Vector[A]) extends AnyVal {
  def random = v.random.get
}

object NonEmptyVector {
  def create[A](v: Vector[A]): Option[NonEmptyVector[A]] =
    if (v.isEmpty) None else Some(NonEmptyVector(v))

  implicit object NonEmptyVectorMonadPlus extends Monad[NonEmptyVector] with Plus[NonEmptyVector] {
    override def bind[A, B](fa: NonEmptyVector[A])(f: (A) => NonEmptyVector[B]): NonEmptyVector[B] =
      NonEmptyVector(fa.v.flatMap(f.andThen(_.v)))

    override def point[A](a: => A): NonEmptyVector[A] = NonEmptyVector(Vector(a))

    override def plus[A](a: NonEmptyVector[A], b: => NonEmptyVector[A]): NonEmptyVector[A] =
      NonEmptyVector(a.v ++ b.v)
  }
}
