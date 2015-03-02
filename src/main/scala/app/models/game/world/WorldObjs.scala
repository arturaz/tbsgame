package app.models.game.world

import app.models.game.world.WObject.Id
import app.models.game.world.WorldObjs.{PositionsMap, ObjectsMap}

import scala.collection.generic.CanBuildFrom
import scala.collection.{mutable, IterableLike}
import scala.reflect.ClassTag
import scalaz.{-\/, \/-, \/}
import implicits._

object WorldObjs {
  type All = WorldObjs[WObject]
  type Any = WorldObjs[_ <: WObject]
  type Static = WorldObjs[WObject.Static]

  def empty[Obj <: WObject] = new WorldObjs[Obj](Map.empty, Map.empty)

  def apply[Obj <: WObject](objects: Obj*): String \/ WorldObjs[Obj] =
    empty[Obj].add(objects)

  private type ObjectsMap[Obj <: WObject] = Map[WObject.Id, Obj]
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

  def newBuilder[Obj <: WObject] = new mutable.Builder[Obj, WorldObjs[Obj]] {
    private[this] var objs = empty[Obj]

    override def +=(elem: Obj) = { objs += elem; this }
    override def result() = objs
    override def clear(): Unit = { objs = empty }
  }

  implicit def canBuildFrom[Obj <: WObject, T]
  : CanBuildFrom[WorldObjs[Obj], T, Vector[T]] =
    new CanBuildFrom[WorldObjs[Obj], T, Vector[T]] {
      override def apply(from: WorldObjs[Obj]) = apply()
      override def apply() = Vector.newBuilder
    }
}

case class WorldObjs[Obj <: WObject] private (
  objectsMap: ObjectsMap[Obj],
  positionsMap: PositionsMap
) extends IterableLike[Obj, WorldObjs[Obj]]{
  import WorldObjs._

  override def toString() = s"WorldObjs(objects: $objectsMap, positions: $positionsMap)"

  private[this] def doManyEither[A]
  (as: TraversableOnce[A])(f: (WorldObjs[Obj], A) => String \/ WorldObjs[Obj])
  : String \/ WorldObjs[Obj] =
    as.foldLeft(this.rightZ[String]) {
      case (\/-(wo), a) => f(wo, a)
      case (left @ -\/(err), _) => left
    }

  def add(obj: Obj): String \/ WorldObjs[Obj] = {
    if (objectsMap.contains(obj.id)) s"$obj already exists in $this".leftZ
    else copy(
      objectsMap = objectsMap + (obj.id -> obj),
      positionsMap = positionsMap |> addPositions(obj.id, obj.bounds.points)
    ).rightZ
  }

  def add(objs: TraversableOnce[Obj]): String \/ WorldObjs[Obj] =
    doManyEither(objs)(_ add _)

  def add_!(obj: Obj): WorldObjs[Obj] = add(obj).right_!
  def +(obj: Obj): WorldObjs[Obj] = add_!(obj)
  
  def add_!(objs: TraversableOnce[Obj]): WorldObjs[Obj] = add(objs).right_!
  def ++(objs: TraversableOnce[Obj]): WorldObjs[Obj] = add_!(objs)

  def remove(objId: WObject.Id): String \/ WorldObjs[Obj] = {
    objectsMap.get(objId).fold2(
      s"$objId is not in $this".leftZ,
      obj => copy(
        objectsMap = objectsMap - obj.id,
        positionsMap = positionsMap |> removePositions(obj.id, obj.bounds.points)
      ).rightZ
    )
  }
  def remove(objs: TraversableOnce[WObject.Id]): String \/ WorldObjs[Obj] =
    doManyEither(objs)(_ remove _)

  def remove_!(obj: WObject.Id): WorldObjs[Obj] = remove(obj).right_!
  def -(obj: WObject.Id): WorldObjs[Obj] = remove_!(obj)

  def remove_!(objs: TraversableOnce[WObject.Id]): WorldObjs[Obj] = remove(objs).right_!
  def --(objs: TraversableOnce[WObject.Id]): WorldObjs[Obj] = remove_!(objs)

  def update[A <: Obj](before: A, after: A): String \/ WorldObjs[Obj] = {
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

  def update_![A <: Obj](before: A, after: A): WorldObjs[Obj] =
    update(before, after).right_!

  def objects = objectsMap.values
  override def size = objectsMap.size
  override def iterator = objectsMap.valuesIterator
  override protected[this] def newBuilder = WorldObjs.newBuilder
  override def seq = objectsMap.values

  /* Keeps world objects that are at least partly in given positions */
  def filterPartial(positions: TraversableOnce[Vect2]): WorldObjs[Obj] = {
    val emptyIds = Set.empty[WObject.Id]
    val ids = positions.flatMap(positionsMap.getOrElse(_, emptyIds)).toSet
    ids.foldLeft(WorldObjs.empty[Obj]) { case (objs, id) => objs + objectsMap(id) }
  }

  override def filter(predicate: Obj => Boolean): WorldObjs[Obj] = {
    objectsMap.valuesIterator.foldLeft(this) { case (fWorldObjs, obj) =>
      if (predicate(obj)) fWorldObjs
      else fWorldObjs remove_! obj.id
    }
  }
  override def filterNot(p: Obj => Boolean) = filter(obj => ! p(obj))

  def idsIn(pos: Vect2) = positionsMap.getOrElse(pos, Set.empty)
  def nonEmptyAt(pos: Vect2) = idsIn(pos).nonEmpty
  def emptyAt(pos: Vect2) = ! nonEmptyAt(pos)
  def objectsIn(pos: Vect2): Set[Obj] = idsIn(pos).map(objectsMap.apply)
  def objectsIn(bounds: Bounds): Set[Obj] = objectsIn(bounds.points)
  def objectsIn(points: TraversableOnce[Vect2]): Set[Obj] =
    points.foldLeft(Set.empty[Obj]) { case (s, v) => s ++ objectsIn(v) }
  def isAllFree(bounds: Bounds): Boolean = isAllFree(bounds.points)
  def isAllFree(points: TraversableOnce[Vect2]): Boolean = ! points.exists(nonEmptyAt)

  @inline def get(id: WObject.Id) = objectsMap.get(id)
  @inline def getCT[A : ClassTag](id: WObject.Id) = get(id).collect { case o: A => o }
  def contains(id: Id): Boolean = objectsMap.contains(id)

  def getE(id: WObject.Id) = get(id).toRight(s"Can't find obj $id in world")
  @inline def get(position: Vect2) = objectsIn(position).headOption
  @inline def getCT[A : ClassTag](position: Vect2) =
    objectsIn(position).collectFirst { case o: A => o }
  def getE(position: Vect2) =
    get(position).toRight(s"Can't find obj @ $position in world")
  def find[A <: Obj](predicate: PartialFunction[Obj, A]): Option[A] =
    objects.collectFirst(predicate)
}
