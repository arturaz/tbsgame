package app.actors

import akka.event.Logging
import akka.http.scaladsl._
import akka.http.scaladsl.model.HttpHeader.ParsingResult
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.typed.ScalaDSL._
import akka.typed._
import argonaut.Argonaut._
import implicits._
import infrastructure.GCM
import launch.RTConfig

import scala.util.Try
import scalaz.Scalaz._

object GCMSender {
  sealed trait In
  case class Send(message: GCM.Message) extends In

  object Internal {
    case class GCMComplete(
      message: GCM.Message, result: Try[HttpResponse]
    ) extends In
  }

  def behaviour(
    authHeader: HttpHeader, httpMaterializer: ActorMaterializer
  ): Behavior[Send] =
    ContextAware[In] { ctx =>
      val untypedSystem = ctx.system.asUntyped
      val http = Http(untypedSystem)
      val log = Logging(untypedSystem, ctx.self.asUntyped)
      val headers = Vector(authHeader)

      Static {
        case Send(m) =>
          log.info("Sending GCM message: {}", m)
          val body = m.asJson.nospaces
          log.debug("GCM message as JSON: {}", body)
          val future = http.singleRequest(HttpRequest(
            HttpMethods.POST, "https://gcm-http.googleapis.com/gcm/send",
            headers, HttpEntity(MediaTypes.`application/json`, body)
          ))(httpMaterializer)

          // Logging isn't thread safe.
          future.onComplete(r => ctx.self ! GCMSender.Internal.GCMComplete(m, r))
        case Internal.GCMComplete(message, result) =>
          log.info("GCM response for {}: {}", message, result)
      }
    }.narrow

  def authHeader(key: RTConfig.GCM.Key) =
    HttpHeader.parse("Authorization", s"key=${key.value}") match {
      case ParsingResult.Ok(header, _) => header.right
      case ParsingResult.Error(error) =>
        s"Cannot turn '$key' into HTTP header: $error".left
    }
}
