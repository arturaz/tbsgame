package implicits.actor

import akka.actor._

private[actor] class UTRefMessageWrapper[A, B](f: (A, ActorRef) => B) extends Actor {
  def receive = { case msg => context.parent ! f(msg.asInstanceOf[A], sender()) }
}