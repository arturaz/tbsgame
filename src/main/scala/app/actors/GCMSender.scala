package app.actors

import akka.typed.ScalaDSL._
import akka.typed._
import argonaut.Argonaut._
import com.ning.http.client.Response
import implicits.actor._
import infrastructure.GCM
import launch.RTConfig

import scala.util.Try
import scalaz.Scalaz._

object GCMSender {
  sealed trait In
  case class Send(message: GCM.Message) extends In

  object Internal {
    case class GCMComplete(
      message: GCM.Message, result: Try[Response]
    ) extends In
  }

  def behaviour(key: RTConfig.GCM.Key): Behavior[Send] =
    ContextAware[In] { ctx =>
      import dispatch._, ctx.executionContext
      val reqBase =
        url("https://gcm-http.googleapis.com/gcm/send")
        .POST
        .setContentType("application/json", "UTF-8")
        .addHeader("Authorization", s"key=${key.value}")
      val log = ctx.createLogging()

      Static {
        case Send(m) =>
          log.info("Sending GCM message: {}", m)
          val body = m.asJson.nospaces
          log.debug("GCM message as JSON: {}", body)
          val req = reqBase << body
          val future = Http(req OK identity)

          // Logging isn't thread safe.
          future.onComplete(r => ctx.self ! GCMSender.Internal.GCMComplete(m, r))
        case Internal.GCMComplete(message, result) =>
          log.info("GCM response for {}: {}", message, result)
      }
    }.narrow
}
