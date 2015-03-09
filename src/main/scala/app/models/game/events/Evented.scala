package app.models.game.events

import implicits._

object Evented {
  def fromTuple[A](t: (A, Events)): Evented[A] = Evented(t._1, t._2)
  def apply[A](value: A, event: Event): Evented[A] = apply(value, Vector(event))
}

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
  def extract[B, C](implicit ev: A <:< Either[B, C]): Either[B, Evented[C]] =
    value.fold(_.left, Evented(_, events).right)
  def extractFlatten[B, C](
    implicit ev: A <:< Either[B, Evented[C]]
  ): Either[B, Evented[C]] =
    extract.right.map(_.flatten)

  def debugStr = s"Evented:\n## value=$value\n## events=\n${events.mkString("### ", "\n### ", "")}"
}
