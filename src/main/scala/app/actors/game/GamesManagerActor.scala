package app.actors.game

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.event.LoggingReceive
import app.actors.NetClient
import app.actors.NetClient.Management.In.JoinGame.Mode
import app.actors.game.GameActor.StartingHuman
import app.models.User
import app.models.game.world.maps.{GameMaps, GameMap, SingleplayerMap, WorldMaterializer}
import app.models.game.world.{ExtractorStats, Resources, World}
import app.models.game.{TurnTimers, Bot, Human, Team}
import implicits._
import utils.data.NonEmptyVector
import scalaz._, Scalaz._

import scala.util.Random

object GamesManagerActor {
  val StartingResources = ExtractorStats.cost * Resources(4)

  sealed trait In
  object In {
    case class Join(user: User, mode: NetClient.Management.In.JoinGame.Mode) extends In
    case class CancelJoinGame(user: User) extends In
  }

  // TODO: proper singleplayer
//  object PVEGame {
//    sealed trait PresetTeam {
//      def gameTeam: Team
//    }
//    object PresetTeam {
//      object Red extends PresetTeam { val gameTeam = Team() }
//      object Blue extends PresetTeam { val gameTeam = Team() }
//    }
//
//    val empty = PVEGame(None, Set.empty, Set.empty)
//  }
//  case class PVEGame(ref: Option[ActorRef], redTeamPlayers: Set[User], blueTeamPlayers: Set[User]) {
//    def giveTeam: PVEGame.PresetTeam =
//      redTeamPlayers.size ?|? blueTeamPlayers.size match {
//        case Ordering.LT => PVEGame.PresetTeam.Red
//        case Ordering.GT => PVEGame.PresetTeam.Blue
//        case Ordering.EQ => if (Random.chance(0.5)) PVEGame.PresetTeam.Red else PVEGame.PresetTeam.Blue
//      }
//
//    def add(user: User, team: PresetTeam): PVEGame = team match {
//      case PresetTeam.Red => copy(redTeamPlayers = redTeamPlayers + user)
//      case PresetTeam.Blue => copy(blueTeamPlayers = blueTeamPlayers + user)
//    }
//  }
}

class GamesManagerActor(maps: GameMaps) extends Actor with ActorLogging {
  import app.actors.game.GamesManagerActor._

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
            "Not joining a new game, because {} is already in a waiting list, ref: {}",
            user, sender()
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
      case Mode.Singleplayer =>
        //launchRandomGenerated(user, client)
        launchPVE(user, client)
      case pvp: Mode.PvP =>
        val updatedWaitingList = waitingList(pvp) :+ ((user, client))
        waitingList += pvp -> updatedWaitingList
        waitingListUser2Mode += user -> pvp
        if (updatedWaitingList.size < pvp.playersNeeded) log.debug(
          "Added {} from {} to {} waiting list: {}",
          user, client, mode, updatedWaitingList
        )
        else fromWaitingList(pvp)
    }
  }

  private[this] def launchPVE(user: User, client: ActorRef) = {
    // TODO: proper PVE
//    val team = pveGame.giveTeam
//    if (pveGame.ref.isEmpty) {
//      val game = createGame(
//        maps.pve.random, Some(TurnTimers.Settings()), Team(),
//        Set(StartingHuman(Human(user, team.gameTeam), StartingResources, client))
//      )
//      pveGame = pveGame.copy(ref = Some(game))
//    }
//    pveGame = pveGame.add(user, team)

    createGame(
      maps.pve.random, None, Team(),
      Set(StartingHuman(Human(user, Team()), StartingResources, client))
    )
  }

  private[this] def launchRandomGenerated(user: User, client: ActorRef) = {
    val materializer = SingleplayerMap { data => implicit log =>
      val npcBot = Bot(data.npcTeam)
      val spawnerBot = Bot(data.npcTeam)
      World.create(
        data.humanTeam, () => npcBot, () => spawnerBot, staticObjectsKnownAtStart = false
      )
    }
    createGame(
      materializer, None, Team(),
      Set(StartingHuman(Human(user, Team()), StartingResources, client))
    )
  }

  private[this] def fromWaitingList(mode: Mode.PvP): Unit = {
    val (entries, newWaitingList) = waitingList(mode).splitAt(mode.playersNeeded)
    waitingList += mode -> newWaitingList
    val teams = Vector.fill(mode.teams)(Team())
    val players = entries.zipWithIndex.map { case ((_user, _client), idx) =>
      val team = teams.wrapped(idx)
      StartingHuman(Human(_user, team), StartingResources, _client)
    }.toSet
    entries.foreach { case (user, _) => waitingListUser2Mode -= user }

    log.debug(
      "Fetched {} from waiting list for mode {}, rest={}", players, mode, newWaitingList
    )
    // TODO: will fail if we have more teams than any of the maps support
    val map = maps.pvp.v.filter(_.startingPositions.size >= mode.teams).random.get
    val npcTeam = Team()

    createGame(map, Some(TurnTimers.Settings()), npcTeam, players)
  }

  private[this] def createGame(
    worldMaterializer: WorldMaterializer, turnTimerSettings: Option[TurnTimers.Settings],
    npcTeam: Team, starting: Set[GameActor.StartingHuman]
  ): ActorRef = {
    val game = context.actorOf(GameActor.props(
      worldMaterializer, turnTimerSettings, npcTeam, starting
    ))
    context.watch(game)
    starting.foreach { data =>
      user2game += data.human.user -> ((game, data.human))
    }
    game2humans += game -> starting.map(_.human)
    log.info("Game {} created for {}", game, starting)
    game
  }
}
