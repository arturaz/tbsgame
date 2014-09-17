package app.algorithms

import app.models.world.units.WUnit
import app.models.world.{Bounds, Fighter, TileRange, Vect2}

import scala.collection.immutable.Queue

/**
 * Created by arturas on 2014-09-15.
 */
object Pathfinding {
  case class SearchRes[+A](value: A, movementNeeded: Int, path: Vector[Vect2])
  private[this] case class Node(
    position: Vect2, distance: Int, cameFrom: Option[Node]
  ) {
    def path: Vector[Vect2] =
      cameFrom.map(_.path).getOrElse(Vector.empty) ++: Vector(position)
  }

  /* Finds objects which can be attacked with fewest moves. */
  def attackSearch[A](
    /* Only objects that are 1x1 sized can use this method. */
    origin: WUnit with Fighter, targets: Iterable[A], obstacles: Set[Bounds]
  )(aToBounds: A => Bounds): Vector[SearchRes[A]] = attackSearch(
    origin.position, origin.movementLeft, origin.stats.attackRange,
    targets, obstacles
  )(aToBounds)

  /* Finds objects which can be attacked with fewest moves. */
  def attackSearch[A](
    /* Only objects that are 1x1 sized can use this method. */
    origin: Fighter, targets: Iterable[A], obstacles: Set[Bounds]
  )(aToBounds: A => Bounds): Vector[SearchRes[A]] = attackSearch(
    origin.position, TileRange(0), origin.stats.attackRange,
    targets, obstacles
  )(aToBounds)

  /* Finds objects which can be attacked with moving. */
  private def attackSearch[A](
    /* Only objects that are 1x1 sized can use this method. */
    origin: Vect2, movementRange: TileRange, attackRange: TileRange,
    targets: Iterable[A], obstacles: Set[Bounds]
  )(aToBounds: A => Bounds): Vector[SearchRes[A]] = {
    def attackableTargets(from: Vect2): Iterable[A] = targets.filter { target =>
      Fighter.canAttack(from, attackRange, aToBounds(target))
    }
    def attackableResults(from: Vect2, currentNode: Node): Map[A, SearchRes[A]] =
      attackableTargets(from).map { target =>
        target -> SearchRes(target, currentNode.distance, currentNode.path)
      }.toMap

    val originNode = Node(origin, 0, None)
    var queue = Queue(originNode)
    var visited = Set.empty[Node]
    var validPositions = Vector.empty[SearchRes[A]]
    var allAttackables = attackableResults(origin, originNode)
    /* Add to attackables overwriting only if it needs less movement. */
    def addAttackables(updates: Map[A, SearchRes[A]]): Unit = {
      allAttackables = updates.foldLeft(allAttackables) {
      case (current, kv @ (target, searchRes)) =>
        current.get(target).fold(current + kv)(curRes =>
          if (curRes.movementNeeded > searchRes.movementNeeded) current + kv
          else current
        )
      }
    }

    while (queue.nonEmpty) {
      val (current, rest) = queue.dequeue
      if (current.distance > movementRange.range || visited.contains(current))
        queue = rest
      else {
        visited += current
        addAttackables(attackableResults(current.position, current))

        def node(v: Vect2) = Node(v, current.distance + 1, Some(current))
        def add(v: Vect2): Unit =
          if (! obstacles.exists(_.contains(v))) queue :+= node(v)

        add(current.position.up)
        add(current.position.right)
        add(current.position.down)
        add(current.position.left)
      }
    }

    allAttackables.view.map(_._2).toVector
  }
}
