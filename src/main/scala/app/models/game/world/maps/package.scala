package app.models.game.world

import app.models.game.events.{WarpZoneChangeEvt, VisibilityChangeEvt}

/**
 * Created by arturas on 2014-10-29.
 */
package object maps {
  type WarpZoneMap = PointOwnerMap[WarpZoneChangeEvt]
  type VisibilityMap = PointOwnerMap[VisibilityChangeEvt]
}
