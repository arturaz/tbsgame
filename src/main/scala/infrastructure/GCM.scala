package infrastructure

import launch.RTConfig
import argonaut._, Argonaut._
import spire.math.UInt

import scala.concurrent.duration.FiniteDuration

object GCM {
  def searchingForOpponent(
    foreground: Data.SearchingForOpponent.InForeground,
    background: Data.SearchingForOpponent.InBackground,
    timeToLive: RTConfig.TTL
  ) = Message(
    "/topics/searching_for_opponent",
    Data.SearchingForOpponent(foreground, background, timeToLive)
  )

  case class Message(to: String, data: Data)
  object Message {
    implicit val jsonEncoder = EncodeJson { (m: Message) =>
      ("to" := m.to) ->:
        "data".:=(m.data)(Data.jsonEncoder) ->:
        ("priority" := "high") ->:
        ("time_to_live" := m.data.timeToLive.duration.toSeconds.toInt) ->:
        jEmptyObject
    }
  }

  sealed trait Data {
    def timeToLive: RTConfig.TTL
  }
  object Data {
    case class SearchingForOpponent(
      foreground: SearchingForOpponent.InForeground, background: SearchingForOpponent.InBackground,
      timeToLive: RTConfig.TTL
    ) extends Data
    object SearchingForOpponent {
      case class InForeground(count: UInt)
      case class InBackground(count: UInt)
    }

    implicit val jsonEncoder = EncodeJson[Data] {
      case m: SearchingForOpponent =>
        ("foreground" := m.foreground.count.signed) ->:
          ("background" := m.background.count.signed) ->:
          jEmptyObject
    }
  }
}