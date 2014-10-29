package app.models.game.world.maps

import app.models.game.events.{VisibilityChangeEvt, WarpZoneChangeEvt}
import app.models.game.world.{Bounds, OwnedObj, PointOwnerMap, WObject}
import implicits._

object VisibilityMap {
  def apply(
    bounds: Bounds, objects: TraversableOnce[WObject]
  ): VisibilityMap =
    PointOwnerMap(bounds, pointsOf _, VisibilityChangeEvt.apply _, objects)

  def pointsOf(obj: OwnedObj) =
    if (obj.isWarpedIn) obj.visibility.points else Iterator(obj.position)
}

object WarpZoneMap {
  def apply(
    bounds: Bounds, objects: TraversableOnce[WObject]
  ): WarpZoneMap =
    PointOwnerMap(bounds, pointsOf _, WarpZoneChangeEvt.apply _, objects)

  def pointsOf(obj: OwnedObj) =
    obj.warpZone.filter(_ => obj.isWarpedIn).fold2(Iterator.empty, _.points)
}