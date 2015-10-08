package app.models.game.events

import implicits._
import scalaz._, Scalaz._

object Evented {
  def fromTuple[A](t: (A, Events)): Evented[A] = Evented(t._1, t._2)
  def apply[A](value: A, event: Event): Evented[A] = apply(value, Vector(event))

  implicit object instances extends Functor[Evented] {
    override def map[A, B](fa: Evented[A])(f: (A) => B) = fa.map(f)
  }
}

/* This is Writer monad specialized for log=Vector[Event] */
case class Evented[+A](value: A, events: Events=Vector.empty) {
  def +:(earlierEvent: Event): Evented[A] = copy(events = earlierEvent +: events)
  def :+(laterEvent: Event): Evented[A] = copy(events = events :+ laterEvent)
  def ++:(earlierEvents: Events): Evented[A] = copy(events = earlierEvents ++ events)
  def :++(laterEvents: Events): Evented[A] = copy(events = events ++ laterEvents)

  /* Map with events. */
  def mapE[B](f: (A, Events) => B) = copy(value = f(value, events))
  def map[B](f: A => B) = mapE { case (v, e) => f(v) }

  def flatMap[B](f: A => Evented[B]) = events ++: f(value)
  def flatMapE[B](f: (A, Events) => Evented[B]) = flatMap(f(_, events))

  def flatten[B](implicit ev: A <:< Evented[B]): Evented[B] = flatMap(ev)
  def extract[B, C](implicit ev: A <:< (B \/ C)): B \/ Evented[C] =
    value.fold(_.left, Evented(_, events).right)

  def extractT1[B, C](implicit ev: A <:< (B, C)): (Evented[B], C) = (map(_._1), value._2)
  def extractT2[B, C](implicit ev: A <:< (B, C)): (B, Evented[C]) = (value._1, map(_._2))

  def extractFlatten[B, C](
    implicit ev: A <:< (B \/ Evented[C])
  ): B \/ Evented[C] = extract.map(_.flatten)

  def debugStr = s"Evented:\n## value=$value\n## events=\n${events.mkString("### ", "\n### ", "")}"
}
