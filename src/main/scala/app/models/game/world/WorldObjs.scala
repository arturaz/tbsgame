package app.models.game.world

import app.models.game.world.WorldObjs.{PositionsMap, ObjectsMap}

import scala.collection.generic.CanBuildFrom
import scala.collection.{mutable, IterableLike}
import scalaz.{-\/, \/-, \/}
import implicits._

object WorldObjs {
  val empty = new WorldObjs(Map.empty, Map.empty)

  def apply(objects: WObject*): String \/ WorldObjs = empty.add(objects)

  private type ObjectsMap = Map[WObject.Id, WObject]
  private type PositionsMap = Map[Vect2, Set[WObject.Id]]

  private def addPositions(
    id: WObject.Id, points: TraversableOnce[Vect2]
  )(positions: PositionsMap): PositionsMap =
    points.foldLeft(positions) { case (fPositions, pos) =>
      fPositions updated (pos, fPositions.getOrElse(pos, Set.empty) + id)
    }

  private def removePositions(
    id: WObject.Id, points: TraversableOnce[Vect2]
  )(positions: PositionsMap): PositionsMap =
    points.foldLeft(positions) { case (fPositions, pos) =>
      if (fPositions contains pos) {
        val newSet = fPositions(pos) - id
        if (newSet.isEmpty) fPositions - pos
        else fPositions updated (pos, newSet)
      }
      else fPositions
    }

  def newBuilder = new mutable.Builder[WObject, WorldObjs] {
    private[this] var objs = empty

    override def +=(elem: WObject) = { objs += elem; this }
    override def result() = objs
    override def clear(): Unit = { objs = empty }
  }

  implicit def canBuildFrom[T]: CanBuildFrom[WorldObjs, T, Vector[T]] =
    new CanBuildFrom[WorldObjs, T, Vector[T]] {
      override def apply(from: WorldObjs) = apply()
      override def apply() = Vector.newBuilder
    }
}

case class WorldObjs private (
  objectsMap: ObjectsMap,
  positionsMap: PositionsMap
) extends IterableLike[WObject, WorldObjs]{
  import WorldObjs._

  override def toString() = s"WorldObjs(objects: $objectsMap, positions: $positionsMap)"

  private[this] def doManyEither
  (objs: TraversableOnce[WObject])(f: (WorldObjs, WObject) => String \/ WorldObjs)
  : String \/ WorldObjs =
    objs.foldLeft(this.rightZ[String]) {
      case (\/-(wo), obj) => f(wo, obj)
      case (left @ -\/(err), _) => left
    }

  def add(obj: WObject): String \/ WorldObjs = {
    if (objectsMap.contains(obj.id)) s"$obj already exists in $this".leftZ
    else copy(
      objectsMap = objectsMap + (obj.id -> obj),
      positionsMap = positionsMap |> addPositions(obj.id, obj.bounds.points)
    ).rightZ
  }

  def add(objs: TraversableOnce[WObject]): String \/ WorldObjs =
    doManyEither(objs)(_ add _)

  def add_!(obj: WObject): WorldObjs = add(obj).right_!
  def +(obj: WObject): WorldObjs = add_!(obj)
  
  def add_!(objs: TraversableOnce[WObject]): WorldObjs = add(objs).right_!
  def ++(objs: TraversableOnce[WObject]): WorldObjs = add_!(objs)

  def remove(obj: WObject): String \/ WorldObjs = {
    if (objectsMap.contains(obj.id)) copy(
      objectsMap = objectsMap - obj.id,
      positionsMap = positionsMap |> removePositions(obj.id, obj.bounds.points)
    ).rightZ
    else s"$obj is not in $this".leftZ
  }
  def remove(objs: TraversableOnce[WObject]): String \/ WorldObjs =
    doManyEither(objs)(_ remove _)

  def remove_!(obj: WObject): WorldObjs = remove(obj).right_!
  def -(obj: WObject): WorldObjs = remove_!(obj)

  def remove_!(objs: TraversableOnce[WObject]): WorldObjs = remove(objs).right_!
  def --(objs: TraversableOnce[WObject]): WorldObjs = remove_!(objs)

  def update[A <: WObject](before: A, after: A): String \/ WorldObjs = {
    if (before.id != after.id)
      s"IDs don't match for [before=$before] and [after=$after]!".leftZ
    else if (! objectsMap.contains(before.id))
      s"$before is not in $this".leftZ
    else {
      val beforePoints = before.bounds.points.toSet
      val afterPoints = after.bounds.points.toSet
      val removedPositions =  beforePoints -- afterPoints
      val addedPositions = afterPoints -- beforePoints
      copy(
        objectsMap = objectsMap + (before.id -> after),
        positionsMap =
          positionsMap |> removePositions(before.id, removedPositions) |>
          addPositions(before.id, addedPositions)
      ).rightZ
    }
  }

  def update_![A <: WObject](before: A, after: A): WorldObjs =
    update(before, after).right_!

  def objects = objectsMap.values
  override def size = objectsMap.size
  override def iterator = objectsMap.valuesIterator
  override protected[this] def newBuilder = WorldObjs.newBuilder
  override def seq = objectsMap.values

  /* Keeps world objects that are at least partly in given positions */
  def filterPartial(positions: TraversableOnce[Vect2]): WorldObjs = {
    val emptyIds = Set.empty[WObject.Id]
    val ids = positions.flatMap(positionsMap.getOrElse(_, emptyIds)).toSet
    ids.foldLeft(WorldObjs.empty) { case (objs, id) => objs + objectsMap(id) }
  }

  override def filter(predicate: WObject => Boolean): WorldObjs = {
    objectsMap.valuesIterator.foldLeft(this) { case (fWorldObjs, obj) =>
      if (predicate(obj)) fWorldObjs
      else fWorldObjs remove_! obj
    }
  }
  override def filterNot(p: (WObject) => Boolean) = filter(obj => ! p(obj))

  def idsIn(pos: Vect2) = positionsMap.getOrElse(pos, Set.empty)
  def contains(pos: Vect2) = idsIn(pos).nonEmpty
  def objectsIn(pos: Vect2): Set[WObject] = idsIn(pos).map(objectsMap.apply)
  def objectsIn(bounds: Bounds): Set[WObject] =
    bounds.points.foldLeft(Set.empty[WObject]) { case (s, v) => s ++ objectsIn(v) }
  def isAllFree(bounds: Bounds): Boolean = isAllFree(bounds.points)
  def isAllFree(points: TraversableOnce[Vect2]): Boolean = ! points.exists(contains)
}
