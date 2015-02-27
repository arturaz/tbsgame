package app.actors.game

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.event.LoggingReceive
import app.actors.NetClient
import app.actors.NetClient.Management.In.JoinGame.Mode
import app.actors.game.GameActor.StartingHuman
import app.models.User
import app.models.game.world.{TileDistance, World, Resources}
import app.models.game.world.buildings.{WarpLinkerStats, ExtractorStats}
import app.models.game.world.units.ScoutStats
import app.models.game.{Human, Bot, Team}
import app.models.game.world.maps.{SingleplayerMap, WorldMaterializer, GameMap}
import implicits._
import utils.data.NonEmptyVector

object GamesManagerActor {
  val Teams = 2
  val PlayersPerTeam = 1
  val PlayersNeeded = Teams * PlayersPerTeam
  val StartingResources = ExtractorStats.cost * Resources(3)

  sealed trait In
  object In {
    case class Join(user: User, mode: NetClient.Management.In.JoinGame.Mode) extends In
    case class CancelJoinGame(user: User) extends In
  }
}

class GamesManagerActor(maps: NonEmptyVector[GameMap]) extends Actor with ActorLogging {
  import GamesManagerActor._

  type WaitingList = Vector[(User, ActorRef)]
  type WaitingLists = Map[Mode.PvP, WaitingList]
  private[this] var waitingList: WaitingLists = Map.empty.withDefaultValue(Vector.empty)
  private[this] var waitingListUser2Mode = Map.empty[User, Mode.PvP]
  private[this] var user2game = Map.empty[User, (ActorRef, Human)]
  private[this] var game2humans = Map.empty[ActorRef, Set[Human]]

  override def supervisorStrategy = OneForOneStrategy() {
    case _ => Stop
  }

  override def receive = LoggingReceive {
    case GamesManagerActor.In.Join(user, mode) =>
      user2game.get(user).fold2(
        {
          if (waitingListUser2Mode.contains(user)) log.warning(
            "Not joining a new game, because {} is already in a waiting list", user
          )
          else noExistingGame(user, mode, sender())
        },
        { case (game, human) => game.tell(GameActor.In.Join(human), sender()) }
      )

    case GamesManagerActor.In.CancelJoinGame(user) =>
      waitingListUser2Mode.get(user).fold2(
        log.info("Not cancelling join game for {}, because not in joining list", user),
        mode => {
          waitingListUser2Mode -= user
          waitingList += mode -> waitingList(mode).filter(_._1 =/= user)
          sender() ! NetClient.Management.Out.JoinGameCancelled
        }
      )

    case Terminated(ref) =>
      game2humans.get(ref).foreach { humans =>
        log.info("Game {} terminated for humans {}", ref, humans)
        game2humans -= ref
        humans.foreach { human => user2game -= human.user }
      }
  }

  private[this] def noExistingGame(user: User, mode: Mode, client: ActorRef): Unit = {
    mode match {
      case Mode.Singleplayer => launchSingleplayer(user, client)
      case pvp: Mode.PvP =>
        val updatedWaitingList = waitingList(pvp) :+ (user, client)
        waitingList += pvp -> updatedWaitingList
        waitingListUser2Mode += user -> pvp
        if (updatedWaitingList.size < pvp.playersNeeded) log.debug(
          "Added {} from {} to {} waiting list: {}",
          user, client, mode, updatedWaitingList
        )
        else fromWaitingList(pvp)
    }
  }

  private[this] def launchSingleplayer(user: User, client: ActorRef) = {
    val materializer = SingleplayerMap { data => implicit log =>
      val npcBot = Bot(data.npcTeam)
      val spawnerBot = Bot(data.npcTeam)
      World.create(data.humanTeam, () => npcBot, () => spawnerBot)
    }
    createGame(
      materializer, Team(),
      Set(StartingHuman(Human(user, Team()), StartingResources, client))
    )
  }

  private[this] def fromWaitingList(mode: Mode.PvP): Unit = {
    val (entries, newWaitingList) = waitingList(mode).splitAt(mode.playersNeeded)
    waitingList += mode -> newWaitingList
    val teams = Vector.fill(Teams)(Team())
    val players = entries.zipWithIndex.map { case ((_user, _client), idx) =>
      val team = teams.wrapped(idx)
      StartingHuman(Human(_user, team), StartingResources, _client)
    }.toSet
    entries.foreach { case (user, _) => waitingListUser2Mode -= user }

    log.debug(
      "Fetched {} from waiting list for mode {}, rest={}", players, mode, newWaitingList
    )
    val map = maps.random
    val npcTeam = Team()

    createGame(map, npcTeam, players)
  }

  private[this] def createGame(
    worldMaterializer: WorldMaterializer, npcTeam: Team,
    starting: Set[GameActor.StartingHuman]
  ): ActorRef = {
    val game = context.actorOf(GameActor.props(worldMaterializer, npcTeam, starting))
    context.watch(game)
    starting.foreach { data =>
      user2game += data.human.user -> (game, data.human)
    }
    game2humans += game -> starting.map(_.human)
    log.info("Game {} created for {}", game, starting)
    game
  }
}
