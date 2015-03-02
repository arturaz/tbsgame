package app.models.game.world

import app.models.game.Team
import app.models.game.events.{WarpZoneChangeEvt, VisibilityChangeEvt}

/**
 * Created by arturas on 2014-10-29.
 */
package object maps {
  type WarpZoneMap = PointOwnerMap[WarpZoneChangeEvt]
  type VisibilityMap = PointOwnerMap[VisibilityChangeEvt]
  /* Last state of static objects that were visible but are no longer visible is stored
     here. */
  type WasVisibleMap = Map[Team, WorldObjs.Static]
}
