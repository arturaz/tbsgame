package app.models.game.world

import app.models.game.world.PointOwnerMap.{EvtFn, PointsFn}
import app.models.game.{Owner, Team}
import app.models.game.events.{Evented, PointOwnershipChangeEvt}
import implicits._

object PointOwnerMap {
  type Underlying = Map[(Vect2, Team), Int]
  type PointsFn = OwnedObj => Iterator[Vect2]
  type EvtFn[Evt] = (Team, Vector[Vect2], Vector[Vect2]) => Evt

  def apply[Evt <: PointOwnershipChangeEvt](
    bounds: Bounds, pointsOf: PointsFn, createEvt: EvtFn[Evt],
    objects: TraversableOnce[WObject]
  ): PointOwnerMap[Evt] = {
    objects.foldLeft(PointOwnerMap(
      bounds, pointsOf, createEvt, Map.empty[(Vect2, Team), Int].withDefaultValue(0)
    )) {
      case (map, obj: OwnedObj) => (map + obj).value
      case (map, _) => map
    }
  }

  private def pointsOf(obj: OwnedObj): Iterator[Vect2] =
    (if (obj.isWarpedIn) obj.visibility else obj.bounds).points

  /* Compact visibility changes into least amount of events possible. */
  private def compact[Evt <: PointOwnershipChangeEvt](
    events: Vector[Evt], createEvt: EvtFn[Evt]
  ): Vector[Evt] = {
    def fold(team: Team, positions: Vector[Vect2], change: Int)(map: Underlying) = {
      positions.foldLeft(map) { case (foldMap, pos) =>
        val key = (pos, team)
        foldMap updated (key, foldMap(key) + change)
      }
    }

    val changes = events.foldLeft(
      Map.empty[(Vect2, Team), Int].withDefaultValue(0)
    ) { case (map, evt) => map |>
      fold(evt.team, evt.ownedVects, 1) |>
      fold(evt.team, evt.unownedVects, -1)
    }

    changes.
      filter { case (_, change) => change =/= 0 }.
      groupBy { case ((_, team), _) => team }.
      mapValues { map =>
        map.view.map { case ((point, _), change) => (point, change) }
      }.
      map { case (team, pointChanges) =>
        def extract(s: Iterable[(Vect2, Int)]) = s.map(_._1).toVector
        val (owned, unowned) =
          pointChanges.partition { case (_, change) => change > 0}
        createEvt(team, extract(owned), extract(unowned))
      }.
      toVector
  }
}

case class PointOwnerMap[Evt <: PointOwnershipChangeEvt] private (
  bounds: Bounds, pointsOf: PointsFn, createEvt: EvtFn[Evt],
  /* Default value = 0 */
  map: PointOwnerMap.Underlying
) {
  import app.models.game.world.PointOwnerMap._

  override lazy val toString = {
    val counts = map.foldLeft(
      Map.empty[Team, Int].withDefaultValue(0)
    ) { case (fMap, ((point, team), _)) => fMap.updated(team, fMap(team) + 1) }
    s"PointOwnerMap($counts)"
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

  def filter(owner: Owner): PointOwnerMap[Evt] =
    copy(map = map.filter { case ((_, team), visibility) =>
      team === owner.team && visibility =/= 0
    })

  def +(obj: OwnedObj): Evented[PointOwnerMap[Evt]] =
    this + (pointsOf(obj), obj.owner.team)
  def +(points: TraversableOnce[Vect2], team: Team): Evented[PointOwnerMap[Evt]] =
    Evented.fromTuple(add(points, team))
  private def add(
    points: TraversableOnce[Vect2], team: Team
  ): (PointOwnerMap[Evt], Vector[PointOwnershipChangeEvt]) =
    update(points, team)(_ + 1)

  def -(obj: OwnedObj): Evented[PointOwnerMap[Evt]] =
    this - (pointsOf(obj), obj.owner.team)
  def -(points: TraversableOnce[Vect2], team: Team): Evented[PointOwnerMap[Evt]] =
    Evented.fromTuple(remove(points, team))
  private def remove(
    points: TraversableOnce[Vect2], team: Team
  ): (PointOwnerMap[Evt], Vector[PointOwnershipChangeEvt]) =
    update(points, team)(_ - 1)

  def updated(before: OwnedObj, after: OwnedObj): Evented[PointOwnerMap[Evt]] =
    updated(
      pointsOf(before), before.owner.team,
      pointsOf(after), after.owner.team
    )
  def updated(
    beforeTO: TraversableOnce[Vect2], beforeTeam: Team,
    afterTO: TraversableOnce[Vect2], afterTeam: Team
  ): Evented[PointOwnerMap[Evt]] = {
    val (before, after) = (beforeTO.toVector, afterTO.toVector)
    if (before === after && beforeTeam === afterTeam) Evented(this)
    else {
      val (map1, evts1) = remove(before, beforeTeam)
      val (map2, evts2) = map1.add(after, afterTeam)
      Evented(map2, compact(evts1 ++ evts2, createEvt))
    }
  }

  private[this] def updated(m: PointOwnerMap.Underlying): PointOwnerMap[Evt] =
    copy(map = m)

  private[this] def update(
    points: TraversableOnce[Vect2], team: Team
  )(f: Int => Int): (PointOwnerMap[Evt], Vector[Evt]) = {
    val (underlying, updateEvents) = points.foldLeft(
      (map, Vector.empty[Evt])
    ) { case (orig @ (m, events), p) =>
      if (bounds.contains(p)) {
        val key = p -> team
        val current = m(key)
        val next = f(current)
        val newEvents =
          if (current === 0 && next =/= 0)
            Vector(createEvt(team, Vector(p), Vector.empty))
          else if (current =/= 0 && next === 0)
            Vector(createEvt(team, Vector.empty, Vector(p)))
          else Vector.empty
        (m updated(key, next), events ++ newEvents)
      }
      else orig
    }

    (updated(underlying), compact(updateEvents, createEvt))
  }
}
