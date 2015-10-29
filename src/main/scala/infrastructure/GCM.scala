package infrastructure

import java.util.UUID

import scala.concurrent.duration._
import launch.RTConfig
import argonaut._, Argonaut._
import implicits._

object GCM {
  case class Message(to: String, data: Data)
  object Message {
    implicit val jsonEncoder = EncodeJson { (m: Message) =>
      ("to" := m.to) ->:
        "data".:=(m.data)(sfoJsonEncoder) ->:
        ("priority" := "high") ->:
        ("time_to_live" := m.data.timeToLive.duration.toSeconds.toInt) ->:
        jEmptyObject
    }
  }

  sealed trait Data {
    def topicName: String
    def topicPath = s"/topics/$topicName"
    def message = Message(topicPath, this)
    def timeToLive: RTConfig.TTL
  }
  case class SearchingForOpponent(state: SearchingForOpponent.State) extends Data {
    override def topicName = "searching_for_opponent"
    override def timeToLive = RTConfig.TTL(0.seconds) // Now or never
  }
  object SearchingForOpponent {
    /**
     * Because GCM can delay our messages, the following scenario might happen:
     *
     * * A player logs in, enters queue, GCM message is sent.
     * * Player leaves queue & exits game.
     * * GCM message arrives saying that we have an opponent waiting for us.
     *
     * Thus Single state is needed so that client can check whether the single opponent is just him
     * and if so - ignore it.
     */
    sealed trait State {
      def asString: String
    }
    object State {
      case object Empty extends State {
        override def asString = "empty"
      }
      case class Single(id: UUID) extends State {
        override def asString = s"single:${id.shortStr}"
      }
      case object Multiple extends State {
        override def asString = "multiple"
      }
    }

    def apply(waitingList: Set[UUID]): SearchingForOpponent = apply(waitingList.size match {
      case 0 => State.Empty
      case 1 => State.Single(waitingList.head)
      case _ => State.Multiple
    })
  }

  implicit val sfoJsonEncoder = EncodeJson[Data] {
    case m: SearchingForOpponent =>
      ("state" := m.state.asString) ->: jEmptyObject
  }
}