package app.models.game.world.maps

import app.models.game.events.{VisibilityChangeEvt, WarpZoneChangeEvt}
import app.models.game.world._
import implicits._

object VisibilityMap {
  def apply(
    bounds: Bounds, objects: WorldObjs
  ): VisibilityMap = PointOwnerMap(
    bounds, pointsOf _, VisibilityChangeEvt.apply _, objects,
    PointOwnerMap.nonSizedLinearBlocking(endPointBlocks = false, _.stats.blocksVisibility)
  )

  def pointsOf(obj: OwnedObj) =
    if (obj.isWarpedIn) obj.visibility.points else Iterator(obj.position)
}

object WarpZoneMap {
  def apply(
    bounds: Bounds, objects: WorldObjs
  ): WarpZoneMap = PointOwnerMap(
    bounds, pointsOf _, WarpZoneChangeEvt.apply _, objects,
    PointOwnerMap.nonSizedLinearBlocking(endPointBlocks = true, _.stats.blocksWarp)
  )

  def pointsOf(obj: OwnedObj) =
    obj.warpZone.filter(_ => obj.isWarpedIn).fold2(Iterator.empty, _.points)
}