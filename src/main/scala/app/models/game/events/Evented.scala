package app.models.game.events

object Evented {
  def fromTuple[A](t: (A, Events)): Evented[A] = Evented(t._1, t._2)
}

case class Evented[+A](value: A, events: Events=Vector.empty) {
  def +:(earlierEvent: Event): Evented[A] = copy(events = earlierEvent +: events)
  def :+(laterEvent: Event): Evented[A] = copy(events = events :+ laterEvent)
  def ++:(earlierEvents: Events): Evented[A] = copy(events = earlierEvents ++ events)
  def :++(laterEvents: Events): Evented[A] = copy(events = events ++ laterEvents)

  /* Map with events. */
  def mapE[B](f: (A, Events) => B) = copy(value = f(value, events))
  def map[B](f: A => B) = mapE { case (v, e) => f(v) }

  def flatMap[B](f: A => Evented[B]) = laterFlatMap(f)
  def laterFlatMap[B](f: A => Evented[B]) = events ++: f(value)

  def earlierFlatMap[B](f: A => Evented[B]) = f(value) :++ events
}
