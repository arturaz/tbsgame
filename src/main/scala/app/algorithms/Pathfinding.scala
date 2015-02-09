package app.algorithms

import app.models.game.world._
import implicits._
import utils.data.NonEmptyVector

import scala.annotation.tailrec
import scala.collection.mutable

object Pathfinding {
  case class SearchRes[+A](value: A, path: Path) {
    def movementNeeded = path.movementNeeded
  }
  /* First vect is always our starting position */
  case class Path private[Pathfinding] (vects: Vector[Vect2]) {
    def limit(movement: Movement) = {
      @tailrec def rec(
        vectsLeft: Vector[Vect2], currentVects: Vector[Vect2],
        currentMovement: Movement
      ): Vector[Vect2] = {
        vectsLeft.headOption match {
          case None => currentVects
          case Some(head) =>
            val movementNeeded = currentVects.lastOption.map(_.movementDistance(head)).
              getOrElse(Movement.zero)
            val newMovement = currentMovement + movementNeeded
            if (newMovement <= movement)
              rec(vectsLeft.tail, currentVects :+ head, newMovement)
            else
              currentVects
        }
      }
      Path(rec(vects, Vector.empty, Movement.zero))
    }

    /* First step only */
    def head = Path(vects.take(2))
    /* Rest of the path if path is longer than 1 step. */
    def tail = if (vects.size > 2) Some(Path(vects.drop(1))) else None

    lazy val movementNeeded = {
      if (vects.size <= 1) Movement.zero
      else vects.sliding(2).map { case Vector(a, b) => a.movementDistance(b)}.sum
    }
  }

  object Path {
    /* Validates whether all vects in the path forms a valid path in the world. */
    def validate(
      world: World, startingPosition: Vect2, path: NonEmptyVector[Vect2]
    ): Either[String, Path] = {
      path.v.foldLeft(startingPosition) { case (p, nextP) =>
        if (! world.bounds.contains(nextP))
          return s"$nextP is not within world bounds ${world.bounds}!".left
        else if (! p.isNextTo(nextP))
          return s"$p is not next to $nextP!".left
        else if (world.findObj(nextP).isDefined)
          return s"$nextP is taken in the world".left
        nextP
      }

      Path(startingPosition +: path.v).right
    }
  }

  private[this] case class Node(
    position: Vect2, distance: Movement, cameFrom: Option[Node]
  ) {
    def path = Path(pathPoints)
    private def pathPoints: Vector[Vect2] =
      cameFrom.fold(Vector.empty[Vect2])(_.pathPoints) ++: Vector(position)
  }

  def movement(
    origin: Movable, worldBounds: Bounds, obstacles: WorldObjs
  ): Vector[Path] = {
    val oNode = originNode(origin.position)
    var nodes = Map.empty[Vect2, Node]
    bfs(oNode, origin.movementLeft, worldBounds, obstacles)(
      _ => false,
      node => {
        val existing = nodes.get(node.position).filter { existingNode =>
          existingNode.distance <= node.distance
        }
        if (existing.isEmpty) nodes += node.position -> node
      }
    )
    nodes.values.filter(_.cameFrom.isDefined).map(_.path).toVector
  }

  /* Finds objects which can be attacked with fewest moves. */
  def attackSearch[A](
    origin: Movable with Fighter, targets: Iterable[A],
    worldBounds: Bounds, obstacles: WorldObjs
  )(aToBounds: A => Bounds): Vector[SearchRes[A]] = attackSearch(
    origin.position, origin.movementLeft, origin.stats.attackRange,
    targets, worldBounds, obstacles
  )(aToBounds)

  /* Finds objects which can be attacked with fewest moves. */
  def attackSearch[A](
    origin: Fighter, targets: Iterable[A], worldBounds: Bounds, obstacles: WorldObjs
  )(aToBounds: A => Bounds): Vector[SearchRes[A]] = attackSearch(
    origin.position, Movement.fromAbsolute(0), origin.stats.attackRange,
    targets, worldBounds, obstacles
  )(aToBounds)

  /* Finds objects which can be attacked with moving. */
  private def attackSearch[A](
    /* Only objects that don't move or are 1x1 sized can use this method. */
    origin: Vect2, movementRange: Movement, attackRange: RadialDistance,
    targets: Iterable[A], worldBounds: Bounds, obstacles: WorldObjs,
    resultsNeeded: Int=Int.MaxValue
  )(aToBounds: A => Bounds): Vector[SearchRes[A]] = {
    def attackableTargets(from: Vect2): Iterable[A] = targets.filter { target =>
      aToBounds(target).withinDistance(from, attackRange)
    }
    def attackableResults(from: Vect2, currentNode: Node): Map[A, SearchRes[A]] =
      attackableTargets(from).map { target =>
        target -> SearchRes(target, currentNode.path)
      }.toMap

    val originNode = this.originNode(origin)
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

    bfs(originNode, movementRange, worldBounds, obstacles)(
      _ => allAttackables.size >= resultsNeeded,
      current => addAttackables(attackableResults(current.position, current))
    )

    allAttackables.view.map(_._2).toVector
  }

  private[this] def originNode(pos: Vect2) = Node(pos, Movement.zero, None)

  private[this] def bfs(
    originNode: Node, movementRange: Movement, worldBounds: Bounds,
    obstacles: WorldObjs
  )(abortSearch: Node => Boolean, onNodeVisited: Node => Unit): Unit = {
    if (movementRange.isNotZero) {
      val (pos, mr) = (originNode.position, movementRange.tileValue)
      val movementBounds =
        Bounds(pos.x - mr to pos.x + 1 + mr, pos.y - mr to pos.y + 1 + mr)
      worldBounds.intersection(movementBounds).foreach { bounds =>
        bfs(originNode, bounds, obstacles)(
          n => n.distance > movementRange || abortSearch(n),
          onNodeVisited
        )
      }
    }
  }

  private[this] def bfs(
    origin: Vect2, worldBounds: Bounds, obstacles: WorldObjs
  )(abortSearch: Node => Boolean, onNodeVisited: Node => Unit): Unit =
    bfs(originNode(origin), worldBounds, obstacles)(abortSearch, onNodeVisited)

  private[this] def bfs(
    originNode: Node, worldBounds: Bounds, obstacles: WorldObjs
  )(abortSearch: Node => Boolean, onNodeVisited: Node => Unit): Unit = {
    implicit val ord = Ordering[Movement].on((n: Node) => n.distance).reverse
    val queue = mutable.PriorityQueue(originNode)
    var visited = Set.empty[Vect2]

    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if (! (abortSearch(current) || visited.contains(current.position))) {
        visited += current.position
        onNodeVisited(current)

        def node(v: Vect2) =
          Node(v, current.distance + current.position.movementDistance(v), Some(current))

        neighbours(current.position, worldBounds, obstacles).map(node).foreach(queue.+=)
      }
    }
  }

  @inline private[this] def obstructed(v: Vect2, obstacles: WorldObjs) =
    obstacles.contains(v)

  private[this] def neighbours(
    current: Vect2, worldBounds: Bounds, obstacles: WorldObjs
  ): Iterator[Vect2] = {
    def add(v: Vect2) =
      if (worldBounds.contains(v) && ! obstructed(v, obstacles)) Iterator(v)
      else Iterator.empty

    Iterator(
      current.up, current.up.right,
      current.right, current.right.down,
      current.down, current.down.left,
      current.left, current.left.up
    ).flatMap(add)
  }

  def aStar(
    unit: Movable, target: Bounds, worldBounds: Bounds,
    obstacles: WorldObjs
  ): Option[Path] = {
    val start = unit.position
    val goal = Iterator.from(1).map { n =>
      target.perimeterN(n).filterNot(obstructed(_, obstacles)) match {
        case p if p.isEmpty => None
        case p => Some(p.minBy(unit.position.movementDistance))
      }
    }.collect { case Some(v) => v }.next()
    aStar(start, goal, worldBounds, obstacles)(_ movementDistance _)
  }

  private[this] def aStar(
    start: Vect2, goal: Vect2, worldBounds: Bounds, obstacles: WorldObjs
  )(heuristicDistance: (Vect2, Vect2) => Movement): Option[Path] = {
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
    var gScore = Map(start -> Movement.zero)
    // Estimated total cost from start to goal through y.
    def estimate(v: Vect2): Movement = gScore(v) + heuristicDistance(v, goal)
    var fScore = Map(start -> estimate(start))

    // The set of tentative nodes to be evaluated, initially containing the start node
    implicit val ord = Ordering[Movement].on((v: Vect2) => estimate(v)).reverse
    val openSet = mutable.PriorityQueue(start)

    while (openSet.nonEmpty) {
      val current = openSet.dequeue()
      if (current === goal) return Some(Path(reconstructPath(cameFrom, goal)))

      closedSet += current

      neighbours(current, worldBounds, obstacles).
        filterNot(closedSet.contains).foreach { neighbor =>
          val tentativeGScore = gScore(current) + current.movementDistance(neighbor)

          val inOpenSet = openSet.exists(_ === neighbor)
          if (! inOpenSet || tentativeGScore < gScore(neighbor)) {
            cameFrom += neighbor -> current
            gScore += neighbor -> tentativeGScore
            fScore += neighbor -> (tentativeGScore + heuristicDistance(neighbor, goal))
            if (! inOpenSet) openSet += neighbor
          }
        }
    }

    None
  }
}
