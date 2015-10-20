package app.actors

import akka.actor.{Actor, ActorLogging}
import argonaut._, Argonaut._
import infrastructure.GCM
import launch.RTConfig
import spray.client.pipelining._
import spray.http._
import spray.httpx.marshalling.Marshaller

import scala.util.Try

class GCMSender(key: RTConfig.GCM.Key) extends Actor with ActorLogging {
  import context.dispatcher

  implicit private[this] val jsonMarshaller =
    Marshaller.delegate[Json, String](ContentTypes.`application/json`)(_.nospaces)

  private[this] val pipeline =
    addHeader("Authorization", s"key=${key.value}") ~> sendReceive

  override def receive: Receive = {
    case m: GCM.Message =>
      log.info("Sending GCM message: {}", m)
      val body = m.asJson
      log.debug("GCM message as JSON: {}", body.nospaces)
      val future = pipeline(Post("https://gcm-http.googleapis.com/gcm/send", body))
      // Logging isn't thread safe.
      future.onComplete(r => self ! GCMSender.Internal.GCMComplete(m, r))
    case GCMSender.Internal.GCMComplete(message, result) =>
      log.info("GCM response for {}: {}", message, result)
  }
}

object GCMSender {
  object Internal {
    case class GCMComplete(message: GCM.Message, result: Try[HttpResponse])
  }
}
