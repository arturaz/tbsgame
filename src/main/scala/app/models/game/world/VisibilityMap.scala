package app.models.game.world

import app.models.game.{Owner, Team}
import app.models.game.events.{Evented, VisibilityChangeEvt}
import implicits._

object VisibilityMap {
  type Underlying = Map[(Vect2, Team), Int]

  def apply(bounds: Bounds, objects: TraversableOnce[WObject]): VisibilityMap = {
    objects.foldLeft(VisibilityMap(
      bounds, Map.empty[(Vect2, Team), Int].withDefaultValue(0)
    )) {
      case (map, obj: OwnedObj) => (map + obj).value
      case (map, _) => map
    }
  }

  private def pointsOf(obj: OwnedObj): Iterator[Vect2] =
    (if (obj.isWarpedIn) obj.visibility else obj.bounds).points

  /* Compact visibility changes into least amount of events possible. */
  private def compact(
    events: Vector[VisibilityChangeEvt]
  ): Vector[VisibilityChangeEvt] = {
    def fold(team: Team, positions: Vector[Vect2], change: Int)(map: Underlying) = {
      positions.foldLeft(map) { case (foldMap, pos) =>
        val key = (pos, team)
        foldMap updated (key, foldMap(key) + change)
      }
    }

    val changes = events.foldLeft(
      Map.empty[(Vect2, Team), Int].withDefaultValue(0)
    ) { case (map, evt) => map |>
      fold(evt.team, evt.visiblePositions, 1) |>
      fold(evt.team, evt.invisiblePositions, -1)
    }

    changes.
      filter { case (_, change) => change =/= 0 }.
      groupBy { case ((_, team), _) => team }.
      mapValues { map =>
        map.view.map { case ((point, _), change) => (point, change) }
      }.
      map { case (team, pointChanges) =>
        def extract(s: Iterable[(Vect2, Int)]) = s.map(_._1).toVector
        val (visible, invisible) =
          pointChanges.partition { case (_, change) => change > 0}
        VisibilityChangeEvt(team, extract(visible), extract(invisible))
      }.
      toVector
  }
}

case class VisibilityMap private (
  bounds: Bounds,
  /* Default value = 0 */
  map: VisibilityMap.Underlying
) {
  import app.models.game.world.VisibilityMap._

  override lazy val toString = {
    val counts = map.foldLeft(
      Map.empty[Team, Int].withDefaultValue(0)
    ) { case (fMap, ((point, team), _)) => fMap.updated(team, fMap(team) + 1) }
    s"VisibilityMap($counts)"
  }

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
    copy(map = map.filter { case ((_, team), visibility) =>
      team === owner.team && visibility =/= 0
    })

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
    if (before === after) Evented(this)
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
    ) { case (orig @ (m, events), p) =>
      if (bounds.contains(p)) {
        val key = p -> team
        val current = m(key)
        val next = f(current)
        val newEvents =
          if (current === 0 && next =/= 0)
            Vector(VisibilityChangeEvt(team, visiblePositions = Vector(p)))
          else if (current =/= 0 && next === 0)
            Vector(VisibilityChangeEvt(team, invisiblePositions = Vector(p)))
          else Vector.empty
        (m updated(key, next), events ++ newEvents)
      }
      else orig
    }

    (updated(underlying), compact(updateEvents))
  }
}
