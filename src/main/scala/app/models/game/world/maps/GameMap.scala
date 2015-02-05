package app.models.game.world.maps

import app.models.game.world.{WObject, Vect2, WorldObjs}

/**
 * Created by arturas on 2015-01-29.
 */
case class GameMap(
  objects: WorldObjs, startingPositions: Set[Vect2]
) {
  def add(obj: WObject) = objects.add(obj).map(o => copy(objects = o))
  def addStarting(vect: Vect2) = copy(startingPositions = startingPositions + vect)
}
