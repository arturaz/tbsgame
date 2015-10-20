package infrastructure

import launch.RTConfig
import argonaut._, Argonaut._

import scala.concurrent.duration.FiniteDuration

object GCM {
  def searchingForOpponent(
    searching: Boolean, timeToLive: RTConfig.TTL
  ) = Message("/topics/searching_for_opponent", Data.SearchingForOpponent(searching, timeToLive))

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
    case class SearchingForOpponent(searching: Boolean, timeToLive: RTConfig.TTL) extends Data

    implicit val jsonEncoder = EncodeJson { (d: Data) =>
      (d match {
        case m: SearchingForOpponent => "searching" := m.searching
      }) ->: jEmptyObject
    }
  }
}