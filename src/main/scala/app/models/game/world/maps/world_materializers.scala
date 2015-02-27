package app.models.game.world.maps

import akka.event.LoggingAdapter
import app.models.game.world.maps.GameMap.NpcF
import app.models.game.{Human, Bot, Owner, Team}
import app.models.game.world._

import scala.util.Random
import scalaz.\/
import implicits._

trait WorldMaterializer {
  def materialize(teams: Iterable[Team], npcTeam: Team)(implicit log: LoggingAdapter)
  : String \/ World
}

object GameMap {
  type NpcF = Bot => WObject
  def empty(bounds: Bounds) = apply(bounds, WorldObjs.empty, Vector.empty, Set.empty)
}

case class GameMap private (
  bounds: Bounds, objects: WorldObjs, npcs: Vector[NpcF],
  startingPositions: Set[Vect2]
) extends WorldMaterializer {
  def add(obj: WObject) = objects.add(obj).map(o => copy(objects = o))
  def addNpc(objF: NpcF) = copy(npcs = npcs :+ objF)
  def addStarting(vect: Vect2) = copy(startingPositions = startingPositions + vect)

  def materialize(teams: Iterable[Team], npcTeam: Team)(implicit log: LoggingAdapter) = {
    if (teams.size > startingPositions.size)
      s"Needed ${teams.size} starting positions, but only had ${startingPositions.size}."
      .leftZ
    else {
      val npcOwner = Bot(npcTeam)
      val objectsE = npcs.foldLeft(objects.rightZ[String]) { (objectsE, materializer) =>
        objectsE.flatMap { objects => objects.add(materializer(npcOwner))}
      }
      val shuffledStartPositions = Random.shuffle(startingPositions.toVector)
      val withBasesE = teams.zipWithIndex.foldLeft(objectsE) { case (objE, (team, idx)) =>
        val position = shuffledStartPositions(idx)
        objE.flatMap { objects => objects.add(WarpGate(position, team)) }
      }
      withBasesE.map(World(bounds, _))
    }
  }
}

object SingleplayerMap {
  case class Data(humanTeam: Team, npcTeam: Team)
}
case class SingleplayerMap(createWorld: SingleplayerMap.Data => LoggingAdapter => World)
extends WorldMaterializer {
  override def materialize(teams: Iterable[Team], npcTeam: Team)
  (implicit log: LoggingAdapter) = {
    if (teams.size != 1) s"Singleplayer teams != 1! $teams".leftZ
    else {
      val team = teams.head
      createWorld(SingleplayerMap.Data(team, npcTeam))(log).rightZ
    }
  }
}