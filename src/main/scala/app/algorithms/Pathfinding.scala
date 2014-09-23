package app.algorithms

import app.models.world._

import scala.collection.immutable.Queue
import scala.collection.mutable

/**
 * Created by arturas on 2014-09-15.
 */
object Pathfinding {
  case class SearchRes[+A](value: A, path: Path) {
    def movementNeeded = path.movementNeeded
  }
  /* First vect is always our starting position */
  case class Path private[Pathfinding] (vects: Vector[Vect2]) {
    def limit(distance: TileDistance) = Path(vects.take(distance.value + 1))
    lazy val movementNeeded = TileDistance(vects.size - 1)
  }

  private[this] case class Node(
    position: Vect2, distance: TileDistance, cameFrom: Option[Node]
  ) {
    def path = Path(pathPoints)
    protected def pathPoints: Vector[Vect2] =
      cameFrom.fold(Vector.empty[Vect2])(_.pathPoints) ++: Vector(position)
  }

  /* Finds objects which can be attacked with fewest moves. */
  def attackSearch[A](
    origin: MovableWObject with Fighter, targets: Iterable[A],
    obstacles: Set[Bounds]
  )(aToBounds: A => Bounds): Vector[SearchRes[A]] = attackSearch(
    origin.position, origin.movementLeft, origin.companion.attackRange,
    targets, obstacles
  )(aToBounds)

  /* Finds objects which can be attacked with fewest moves. */
  def attackSearch[A](
    origin: Fighter, targets: Iterable[A], obstacles: Set[Bounds]
  )(aToBounds: A => Bounds): Vector[SearchRes[A]] = attackSearch(
    origin.position, TileDistance(0), origin.companion.attackRange,
    targets, obstacles
  )(aToBounds)

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
        target -> SearchRes(target, currentNode.path)
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

    if (movementRange > TileDistance(0)) {
      val worldBounds = Bounds(origin, Vect2(movementRange.value, movementRange.value))
      bfs(originNode, worldBounds, obstacles)(
        allAttackables.size >= resultsNeeded || _.distance > movementRange,
        current => addAttackables(attackableResults(current.position, current))
      )
    }

    allAttackables.view.map(_._2).toVector
  }

  private[this] def originNode(pos: Vect2) = Node(pos, TileDistance(0), None)

  private[this] def bfs(
    origin: Vect2, worldBounds: Bounds, obstacles: Set[Bounds]
  )(abortSearch: Node => Boolean, onNodeVisited: Node => Unit): Unit =
    bfs(originNode(origin), worldBounds, obstacles)(abortSearch, onNodeVisited)

  private[this] def bfs(
    originNode: Node, worldBounds: Bounds, obstacles: Set[Bounds]
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

        neighbours(current.position, worldBounds, obstacles).
          map(node).foreach(queue :+= _)
      }
    }
  }

  private[this] def obstructed(v: Vect2, obstacles: Set[Bounds]) =
    obstacles.exists(_.contains(v))

  private[this] def neighbours(
    current: Vect2, worldBounds: Bounds, obstacles: Set[Bounds]
  ): Iterator[Vect2] = {
    def add(v: Vect2) =
      if (worldBounds.contains(v) && ! obstructed(v, obstacles)) Iterator(v)
      else Iterator.empty

    Iterator(
      current.up, current.right, current.down, current.left
    ).flatMap(add)
  }

  def aStar(
    unit: MovableWObject, target: Bounds, worldBounds: Bounds,
    obstacles: Set[Bounds]
  ): Option[Path] = {
    val start = unit.position
    val goal = Iterator.from(1).map { n =>
      target.perimeterN(n).filterNot(obstructed(_, obstacles)) match {
        case p if p.isEmpty => None
        case p => Some(p.minBy(unit.position.tileDistance))
      }
    }.collect { case Some(v) => v }.next()
    aStar(start, goal, worldBounds, obstacles)(_ tileDistance _)
  }

  private[this] def aStar(
    start: Vect2, goal: Vect2, worldBounds: Bounds, obstacles: Set[Bounds]
  )(h: (Vect2, Vect2) => TileDistance): Option[Path] = {
    def reconstructPath(
      cameFrom: Map[Vect2, Vect2], currentNode: Vect2
    ): Vector[Vect2] = {
      cameFrom.get(currentNode).fold(
        Vector(currentNode)
      )(reconstructPath(cameFrom, _) :+ currentNode)
    }

    // The set of nodes already evaluated.
    var closedSet = Set.empty[Vect2]
    // The map of navigated nodes.
    var cameFrom = Map.empty[Vect2, Vect2]
    // Cost from start along best known path.
    var gScore = Map(start -> TileDistance(0))
    // Estimated total cost from start to goal through y.
    def estimate(v: Vect2): TileDistance = gScore(v) + h(v, goal)
    var fScore = Map(start -> estimate(start))

    // The set of tentative nodes to be evaluated, initially containing the start node
    implicit val ord = Ordering[TileDistance].on((v: Vect2) => estimate(v)).reverse
    val openSet = mutable.PriorityQueue(start)

    while (openSet.nonEmpty) {
      val current = openSet.dequeue()
      if (current == goal) return Some(Path(reconstructPath(cameFrom, goal)))

      closedSet += current

      neighbours(current, worldBounds, obstacles).
        filterNot(closedSet.contains).foreach { neighbor =>
          val tentativeGScore = gScore(current) + TileDistance(1)

          val inOpenSet = openSet.exists(_ == neighbor)
          if (! inOpenSet || tentativeGScore < gScore(neighbor)) {
            cameFrom += neighbor -> current
            gScore += neighbor -> tentativeGScore
            fScore += neighbor -> (tentativeGScore + h(neighbor, goal))
            if (! inOpenSet) openSet += neighbor
          }
        }
    }

    None
  }
}
