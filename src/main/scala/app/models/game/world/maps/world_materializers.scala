package app.models.game.world.maps

import akka.event.LoggingAdapter
import app.models.game._
import app.models.game.world._
import utils.data.NonEmptyVector

import scala.util.Random
import scalaz._, Scalaz._, scalaz.effect._
import implicits._

trait WorldMaterializer {
  def materialize(teams: Iterable[Team])(implicit log: LoggingAdapter): String \/ World
}

object GameMap {
  def empty(bounds: Bounds) = apply(bounds, WorldObjs.empty, Set.empty)
}

case class GameMap private (
  bounds: Bounds, objects: WorldObjs.All, startingPositions: Set[Vect2]
) extends WorldMaterializer {
  def add(obj: WObject) = objects.add(obj).map(o => copy(objects = o))
  def addStarting(vect: Vect2) = copy(startingPositions = startingPositions + vect)

  def materialize(teams: Iterable[Team])(implicit log: LoggingAdapter) = {
    if (teams.size > startingPositions.size)
      s"Needed ${teams.size} starting positions, but only had ${startingPositions.size}."
      .left
    else {
      val shuffledStartPositions = Random.shuffle(startingPositions.toVector)
      val withBasesE = teams.zipWithIndex.foldLeft(objects.right[String]) {
        case (objE, (team, idx)) =>
          val position = shuffledStartPositions(idx)
          objE.flatMap { _.add(WarpGate(position, team)) }
      }
      withBasesE.map(World(bounds, _, staticObjectsKnownAtStart = true))
    }
  }
}

case class GameMaps(pve: NonEmptyVector[GameMap], pvp: Map[Int, NonEmptyVector[GameMap]]) {
  val maxPvpPlayers = pvp.keys.max

  def pvpMapFor(playerCount: Int): GameMaps.NoMapsForXPlayers \/ IO[GameMap] = {
    (playerCount to maxPvpPlayers).foreach { count =>
      pvp.get(count).foreach { maps =>
        return maps.v.randomIO.map(_.get).right
      }
    }
    GameMaps.NoMapsForXPlayers(playerCount, maxPvpPlayers).left
  }
}
object GameMaps {
  case class NoMapsForXPlayers(needed: Int, max: Int) {
    override def toString = s"No maps for $needed players, max supported: $max"
  }
}

object SingleplayerMap {
  case class Data(humanTeam: Team)
}
case class SingleplayerMap(createWorld: SingleplayerMap.Data => LoggingAdapter => World)
extends WorldMaterializer {
  override def materialize(teams: Iterable[Team])
  (implicit log: LoggingAdapter) = {
    if (teams.size != 1) s"Singleplayer teams != 1! $teams".left
    else {
      val team = teams.head
      createWorld(SingleplayerMap.Data(team))(log).right
    }
  }
}