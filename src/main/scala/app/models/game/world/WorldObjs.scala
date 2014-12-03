package app.models.game.world

import app.models.game.world.WorldObjs.{PositionsMap, ObjectsMap}

import scala.collection.generic.CanBuildFrom
import scala.collection.{mutable, IterableLike}
import scalaz.{-\/, \/-, \/}
import implicits._

object WorldObjs {
  val empty = new WorldObjs(Map.empty, Map.empty)

  def apply(objects: WObject*): String \/ WorldObjs =
    objects.foldLeft(empty.rightZ[String]) {
      case (\/-(wo), obj) => wo add obj
      case (left @ -\/(err), _) => left
    }

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
      fPositions updated (pos, fPositions(pos) - id)
    }

  def newBuilder = new mutable.Builder[WObject, WorldObjs] {
    private[this] var objs = empty

    override def +=(elem: WObject) = { objs += elem; this }
    override def result() = objs
    override def clear(): Unit = { objs = empty }
  }

//  implicit def canBuildFromWObj[T <: WObject]: CanBuildFrom[WorldObjs, T, Set[T]] =
//    new CanBuildFrom[WorldObjs, T, Set[T]] {
//      override def apply(from: WorldObjs) = apply()
//      override def apply() = Set.newBuilder
//    }

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

  def add(obj: WObject): String \/ WorldObjs = {
    if (objectsMap.contains(obj.id)) s"$obj already exists in $this".leftZ
    else copy(
      objectsMap = objectsMap + (obj.id -> obj),
      positionsMap = positionsMap |> addPositions(obj.id, obj.bounds.points)
    ).rightZ
  }

  def add_!(obj: WObject): WorldObjs = add(obj).right_!
  def +(obj: WObject): WorldObjs = add_!(obj)

  def remove(obj: WObject): String \/ WorldObjs = {
    if (objectsMap.contains(obj.id)) copy(
      objectsMap = objectsMap - obj.id,
      positionsMap = positionsMap |> removePositions(obj.id, obj.bounds.points)
    ).rightZ
    else s"$obj is not in $this".leftZ
  }

  def remove_!(obj: WObject): WorldObjs = remove(obj).right_!
  def -(obj: WObject): WorldObjs = remove_!(obj)

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

  def filter(positions: TraversableOnce[Vect2]): WorldObjs = {
    positions.foldLeft(WorldObjs.empty) { case (fWorldObjs, pos) =>
      this.positionsMap.get(pos).fold2(
        fWorldObjs,
        ids => fWorldObjs.copy(
          objectsMap = ids.foldLeft(fWorldObjs.objectsMap) { case (objs, id) =>
            objs + (id -> objectsMap(id))
          },
          positionsMap = fWorldObjs.positionsMap + (pos -> ids)
        )
      )
    }
  }

  override def filter(predicate: WObject => Boolean): WorldObjs = {
    objectsMap.valuesIterator.foldLeft(this) { case (fWorldObjs, obj) =>
      if (predicate(obj)) fWorldObjs
      else fWorldObjs remove_! obj
    }
  }

  def idsIn(pos: Vect2) = positionsMap.getOrElse(pos, Set.empty)
  def contains(pos: Vect2) = idsIn(pos).nonEmpty
  def objectsIn(pos: Vect2) = idsIn(pos).map(objectsMap.apply)
  def isAllFree(bounds: Bounds): Boolean = isAllFree(bounds.points)
  def isAllFree(points: TraversableOnce[Vect2]): Boolean = ! points.exists(contains)
}
