package implicits.actor

import akka.actor.ActorRef

case class TypedUntypedActorBridge(raw: ActorRef, sender: ActorRef) {
  def !(msg: Any) = raw.tell(msg, sender)
}
