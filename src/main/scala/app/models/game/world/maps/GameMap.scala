package app.models.game.world.maps

import app.models.game.world.maps.GameMap.NpcF
import app.models.game.{Bot, Owner, Team}
import app.models.game.world._

import scala.util.Random
import scalaz.\/
import implicits._

object GameMap {
  type NpcF = Bot => WObject
  def empty(bounds: Bounds) = apply(bounds, WorldObjs.empty, Vector.empty, Set.empty)
}

case class GameMap private (
  bounds: Bounds, objects: WorldObjs, npcs: Vector[NpcF],
  startingPositions: Set[Vect2]
) {
  def add(obj: WObject) = objects.add(obj).map(o => copy(objects = o))
  def addNpc(objF: NpcF) = copy(npcs = npcs :+ objF)
  def addStarting(vect: Vect2) = copy(startingPositions = startingPositions + vect)

  def materialize(teams: Iterable[Team], npcOwner: Bot): String \/ World = {
    if (teams.size > startingPositions.size)
      s"Needed ${teams.size} starting positions, but only had ${startingPositions.size}."
      .leftZ
    else {
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
