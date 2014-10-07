package app.models.world

import app.models.{Owner, Team}
import app.models.game.events.{Evented, VisibilityChangeEvt}

object VisibilityMap {
  type Underlying = Map[(Vect2, Team), Int]

  def apply(objects: TraversableOnce[WObject]): VisibilityMap = {
    objects.foldLeft(VisibilityMap(Map.empty[(Vect2, Team), Int].withDefaultValue(0))) {
      case (map, obj: OwnedObj) => (map + obj).value
      case (map, _) => map
    }
  }

  private def pointsOf(obj: OwnedObj): Iterator[Vect2] =
    (if (obj.isWarpedIn) obj.visibility else obj.bounds).points

  /* Given visibility changes +1,-1,+1, return just +1 */
  private def compact(
    events: Vector[VisibilityChangeEvt]
  ): Vector[VisibilityChangeEvt] = {
    val changes = events.foldLeft(
      Map.empty[(Vect2, Team), Int].withDefaultValue(0)
    ) { case (map, evt) =>
      evt.positions.foldLeft(map) { case (foldMap, pos) =>
        val key = (pos, evt.team)
        foldMap updated (key, foldMap(key) + (if (evt.visible) 1 else -1))
      }
    }

    changes.groupBy {
      case ((pos, team), change) => (team, if (change == 0) None else Some(change > 0))
    }.to[Vector].flatMap {
      case ((team, Some(visible)), map) =>
        Some(VisibilityChangeEvt(team, map.map(_._1._1).toVector, visible = true))
      case _ =>
        None
    }
  }
}

case class VisibilityMap private (
  /* Default value = 0 */
  map: VisibilityMap.Underlying
) {
  import app.models.world.VisibilityMap._

  def isVisible(team: Team, point: Vect2): Boolean = map((point, team)) > 0
  def isVisible(owner: Owner, point: Vect2): Boolean = isVisible(owner.team, point)

  def isVisiblePartial(owner: Owner, bounds: Bounds): Boolean =
    isVisiblePartial(owner.team, bounds)
  def isVisiblePartial(team: Team, bounds: Bounds): Boolean =
    bounds.points.exists(isVisible(team, _))

  def isVisibleFull(owner: Owner, bounds: Bounds): Boolean =
    isVisibleFull(owner.team, bounds)
  def isVisibleFull(team: Team, bounds: Bounds): Boolean =
    bounds.points.forall(isVisible(team, _))

  def filter(owner: Owner): VisibilityMap =
    copy(map = map.filterKeys { case (_, team) => team == owner.team })

  def +(obj: OwnedObj): Evented[VisibilityMap] =
    this + (pointsOf(obj), obj.owner.team)
  def +(points: TraversableOnce[Vect2], team: Team): Evented[VisibilityMap] =
    Evented.fromTuple(add(points, team))
  private def add(
    points: TraversableOnce[Vect2], team: Team
  ): (VisibilityMap, Vector[VisibilityChangeEvt]) =
    update(points, team)(_ + 1)

  def -(obj: OwnedObj): Evented[VisibilityMap] =
    this - (pointsOf(obj), obj.owner.team)
  def -(points: TraversableOnce[Vect2], team: Team): Evented[VisibilityMap] =
    Evented.fromTuple(remove(points, team))
  private def remove(
    points: TraversableOnce[Vect2], team: Team
  ): (VisibilityMap, Vector[VisibilityChangeEvt]) =
    update(points, team)(_ - 1)

  def updated(before: OwnedObj, after: OwnedObj): Evented[VisibilityMap] =
    updated(
      pointsOf(before), before.owner.team,
      pointsOf(after), after.owner.team
    )
  def updated(
    beforeTO: TraversableOnce[Vect2], beforeTeam: Team,
    afterTO: TraversableOnce[Vect2], afterTeam: Team
  ): Evented[VisibilityMap] = {
    val (before, after) = (beforeTO.toVector, afterTO.toVector)
    if (before == after) Evented(this)
    else {
      val (map1, evts1) = remove(before, beforeTeam)
      val (map2, evts2) = map1.add(after, afterTeam)
      Evented(map2, compact(evts1 ++ evts2))
    }
  }

  private[this] def updated(m: VisibilityMap.Underlying) = copy(map = m)

  private[this] def update(
    points: TraversableOnce[Vect2], team: Team
  )(f: Int => Int): (VisibilityMap, Vector[VisibilityChangeEvt]) = {
    val (underlying, updateEvents) = points.foldLeft(
      (map, Vector.empty[VisibilityChangeEvt])
    ) { case ((m, events), p) =>
      val key = p -> team
      val current = m(key)
      val next = f(current)
      val newEvents =
        if (current == 0 && next != 0)
          Vector(VisibilityChangeEvt(team, Vector(p), visible = true))
        else if (current != 0 && next == 0)
          Vector(VisibilityChangeEvt(team, Vector(p), visible = false))
        else Vector.empty
      (m updated(key, next), events ++ newEvents)
    }

    (updated(underlying), compact(updateEvents))
  }
}
