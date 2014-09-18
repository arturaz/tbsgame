package app.algorithms

import app.models.world._

import scala.collection.immutable.Queue

/**
 * Created by arturas on 2014-09-15.
 */
object Pathfinding {
  case class SearchRes[+A](value: A, movementNeeded: TileDistance, path: Path)
  /* First vect is always our starting position */
  case class Path private (vects: Vector[Vect2]) {
    def limit(distance: TileDistance) = Path(vects.take(distance.value + 1))
  }

  private[this] case class Node(
    position: Vect2, distance: TileDistance, cameFrom: Option[Node]
  ) {
    def path = Path(pathPoints)
    protected def pathPoints: Vector[Vect2] =
      cameFrom.map(_.pathPoints).getOrElse(Vector.empty) ++: Vector(position)
  }

  /* Finds objects which can be attacked with fewest moves. */
  def attackSearch[A](
    origin: MovableWObject with Fighter, targets: Iterable[A],
    obstacles: Set[Bounds]
  )(aToBounds: A => Bounds): Vector[SearchRes[A]] = attackSearch(
    origin.position, origin.movementLeft, origin.stats.attackRange,
    targets, obstacles
  )(aToBounds)

  /* Finds objects which can be attacked with fewest moves. */
  def attackSearch[A](
    origin: Fighter, targets: Iterable[A], obstacles: Set[Bounds]
  )(aToBounds: A => Bounds): Vector[SearchRes[A]] = attackSearch(
    origin.position, TileDistance(0), origin.stats.attackRange,
    targets, obstacles
  )(aToBounds)

  /* Finds an object which can be attacked with fewest moves. Object does not
     have to be in attack range. */
  def longRangeNearestAttackSearch[A](
    origin: MovableWObject with Fighter, targets: Iterable[A], obstacles: Set[Bounds]
  )(aToBounds: A => Bounds): Option[SearchRes[A]] = attackSearch(
    origin.position, {
      val bounds = targets.map(aToBounds).reduce(_ join _)
      bounds.corners.map(origin.position.tileDistance).max
    }, origin.stats.attackRange, targets, obstacles, resultsNeeded = 1
  )(aToBounds).headOption

  /* Finds objects which can be attacked with moving. */
  private def attackSearch[A](
    /* Only objects that don't move or are 1x1 sized can use this method. */
    origin: Vect2, movementRange: TileDistance, attackRange: TileDistance,
    targets: Iterable[A], obstacles: Set[Bounds], resultsNeeded: Int=Int.MaxValue
  )(aToBounds: A => Bounds): Vector[SearchRes[A]] = {
    def attackableTargets(from: Vect2): Iterable[A] = targets.filter { target =>
      aToBounds(target).withinTileDistance(from, attackRange)
    }
    def attackableResults(from: Vect2, currentNode: Node): Map[A, SearchRes[A]] =
      attackableTargets(from).map { target =>
        target -> SearchRes(target, currentNode.distance, currentNode.path)
      }.toMap

    val originNode = this.originNode(origin)
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

    bfs(originNode, obstacles)(
      allAttackables.size >= resultsNeeded || _.distance > movementRange,
      current => addAttackables(attackableResults(current.position, current))
    )

    allAttackables.view.map(_._2).toVector
  }

  private[this] def originNode(pos: Vect2) = Node(pos, TileDistance(0), None)

  private[this] def bfs(
    origin: Vect2, obstacles: Set[Bounds]
  )(abortSearch: Node => Boolean, onNodeVisited: Node => Unit): Unit =
    bfs(originNode(origin), obstacles)(abortSearch, onNodeVisited)

  private[this] def bfs(
    originNode: Node, obstacles: Set[Bounds]
  )(abortSearch: Node => Boolean, onNodeVisited: Node => Unit): Unit = {
    var queue = Queue(originNode)
    var visited = Set.empty[Node]

    while (queue.nonEmpty) {
      val (current, rest) = queue.dequeue
      if (abortSearch(current) || visited.contains(current))
        queue = rest
      else {
        visited += current
        onNodeVisited(current)

        def node(v: Vect2) =
          Node(v, current.distance + TileDistance(1), Some(current))
        def add(v: Vect2): Unit =
          if (! obstacles.exists(_.contains(v))) queue :+= node(v)

        add(current.position.up)
        add(current.position.right)
        add(current.position.down)
        add(current.position.left)
      }
    }
  }
}
