package utils.actors

import akka.actor.{ActorRef, Actor}

trait ProxyUnknown { _: Actor =>
  protected def proxyUnknown(endpointA: ActorRef, endpointB: ActorRef): Receive = {
    case msg =>
      (if (sender() == endpointA) endpointB else endpointA).tell(msg, sender())
  }
}
