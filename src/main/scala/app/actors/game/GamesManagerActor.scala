package app.actors.game

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.event.LoggingReceive
import app.actors.game.GameActor.StartingHuman
import app.models.User
import app.models.game.world.Resources
import app.models.game.world.buildings.{WarpLinkerStats, ExtractorStats}
import app.models.game.world.units.ScoutStats
import app.models.game.{Human, Bot, Team}
import app.models.game.world.maps.GameMap
import implicits._
import utils.data.NonEmptyVector

object GamesManagerActor {
  val Teams = 2
  val PlayersPerTeam = 1
  val PlayersNeeded = Teams * PlayersPerTeam
  val StartingResources = ExtractorStats.cost * Resources(3)

  sealed trait In
  object In {
    case class Join(user: User) extends In
  }
}

class GamesManagerActor(maps: NonEmptyVector[GameMap]) extends Actor with ActorLogging {
  import GamesManagerActor._

  private[this] var waitingList = Vector.empty[(User, ActorRef)]
  private[this] var user2game = Map.empty[User, (ActorRef, Human)]
  private[this] var game2humans = Map.empty[ActorRef, Set[Human]]

  override def supervisorStrategy = OneForOneStrategy() {
    case _ => Stop
  }

  override def receive = LoggingReceive {
    case GamesManagerActor.In.Join(user) =>
      user2game.get(user).fold2(
        noExistingGame(user, sender()),
        { case (game, human) => game.tell(GameActor.In.Join(human), sender()) }
      )
    case Terminated(ref) =>
      game2humans.get(ref).foreach { humans =>
        log.info("Game {} terminated for humans {}", ref, humans)
        game2humans -= ref
        humans.foreach { human => user2game -= human.user }
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
      user2game += data.human.user -> (game, data.human)
    }
    game2humans += game -> starting.map(_.human)
    log.info("Game {} created for {}", game, starting)
    game
  }
}
