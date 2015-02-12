package app.actors.game

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.event.LoggingReceive
import app.actors.game.GameActor.StartingHuman
import app.models.User
import app.models.game.world.Resources
import app.models.game.{Human, Bot, Team}
import app.models.game.world.maps.GameMap
import implicits._
import utils.data.NonEmptyVector

object GamesManagerActor {
  val Teams = 2
  val PlayersPerTeam = 1
  val PlayersNeeded = Teams * PlayersPerTeam
  val StartingResources = Resources(40)
}

class GamesManagerActor(maps: NonEmptyVector[GameMap]) extends Actor with ActorLogging {
  import GamesManagerActor._

  private[this] var waitingList = Vector.empty[(User, ActorRef)]
  private[this] var user2game = Map.empty[User, ActorRef]
  private[this] var game2user = Map.empty[ActorRef, User]

  override def supervisorStrategy = OneForOneStrategy() {
    case _ => Stop
  }

  override def receive = LoggingReceive {
    case msg @ GameActor.In.Join(user) =>
      user2game.get(user).fold2(noExistingGame(user, sender()), _.tell(msg, sender()))
    case Terminated(ref) =>
      game2user.get(ref).foreach { user =>
        log.info("Game {} terminated for user {}", ref, user)
        game2user -= ref
        user2game -= user
      }
  }

  private[this] def noExistingGame(user: User, client: ActorRef): Unit = {
    waitingList :+= (user, client)
    if (waitingList.size < PlayersNeeded) {
      log.debug("Added {} from {} to waiting list: {}", user, client, waitingList)
    }
    else fromWaitingList()
  }

  private[this] def fromWaitingList(): Unit = {
    val (entries, newWaitingList) = waitingList.splitAt(PlayersNeeded)
    waitingList = newWaitingList
    val teams = Vector.fill(Teams)(Team())
    val players = entries.zipWithIndex.map { case ((_user, _client), idx) =>
      val team = teams.wrapped(idx)
      StartingHuman(Human(_user, team), StartingResources, _client)
    }.toSet

    log.debug(s"Fetched {} from waiting list, rest={}", players, waitingList)
    val map = maps.random
    val aiTeam = Team()

    createGame(map, aiTeam, players)
  }

  private[this] def createGame(
    map: GameMap, aiTeam: Team, starting: Set[GameActor.StartingHuman]
  ): ActorRef = {
    val game = context.actorOf(GameActor.props(map, aiTeam, starting))
    context.watch(game)
    starting.foreach { data =>
      val user = data.human.user
      user2game += user -> game
      game2user += game -> user
    }
    log.info("Game {} created for {}", game, starting)
    game
  }
}
