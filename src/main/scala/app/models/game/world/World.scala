package app.models.game.world

import akka.event.LoggingAdapter
import app.models.game._
import app.models.game.ai.GrowingSpawnerAI
import app.models.game.events._
import app.models.game.world.WObject.Id
import app.models.game.world.buildings.{Spawner, WarpGate}
import app.models.game.world.maps.{VisibilityMap, WarpZoneMap}
import app.models.game.world.props.Asteroid
import app.models.game.world.units.{Fortress, RayShip, Wasp}
import implicits._
import infrastructure.PrefixedLoggingAdapter

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Random

case class World private (
  bounds: Bounds, objects: WorldObjs,
  resourcesMap: Map[Player, Resources],
  warpZoneMap: WarpZoneMap, visibilityMap: VisibilityMap
) extends TurnBased[World] {
  import app.models.game.world.World._

  override def toString = s"World($bounds, objects: ${objects.size})"

  def gameTurnStarted(implicit log: LoggingAdapter) =
    Evented(this) |> gameTurnX(_.gameTurnStarted)
  def gameTurnFinished(implicit log: LoggingAdapter) =
    Evented(this) |> gameTurnX(_.gameTurnFinished)
  def teamTurnStarted(team: Team)(implicit log: LoggingAdapter) =
    Evented(this, Vector(TurnStartedEvt(team))) |> teamTurnX(team)(_.teamTurnStarted) |>
    runAI(team)
  def teamTurnFinished(team: Team)(implicit log: LoggingAdapter) =
    Evented(this, Vector(TurnEndedEvt(team))) |> teamTurnX(team)(_.teamTurnFinished)

  def actionsFor(player: Player): Actions = {
    val actions = objects.collect {
      case obj: GivingActions if obj.owner.isFriendOf(player) =>
        obj.companion.actionsGiven
    }
    val total = actions.sum
    total
  }

  private[this] def changeWithMapUpdates(obj: WObject)(
    fo: (WorldObjs, WObject) => WorldObjs,
    fwz: (WarpZoneMap, OwnedObj) => Evented[WarpZoneMap],
    fvis: (VisibilityMap, OwnedObj) => Evented[VisibilityMap]
  ): Evented[World] = {
    val newWorld = copy(objects = fo(objects, obj))
    obj.asOwnedObj.fold2(
      Evented(newWorld),
      oo => for {
        visibilityMap <- fvis(visibilityMap, oo)
        warpZoneMap <- fwz(warpZoneMap, oo)
      } yield newWorld.copy(
        warpZoneMap = warpZoneMap,
        visibilityMap = visibilityMap
      )
    )
  }

  def add(obj: WObject): Evented[World] = {
    changeWithMapUpdates(obj)(_ + _, _ + _, _ + _)
  }
  def remove(obj: WObject): Evented[World] =
    changeWithMapUpdates(obj)(_ - _, _ - _, _ - _)
  def removeEvt(obj: WObject): Evented[World] =
    Evented(this, ObjDestroyedEvt(this, obj)).flatMap(_.remove(obj))

  def updated[A <: WObject](before: A, after: A): Evented[World] = {
    val newWorld = copy(objects = objects update_! (before, after))
    (before.asOwnedObj, after.asOwnedObj) match {
      case (Some(bOO), Some(aOO)) =>
        for {
          visibilityMap <- visibilityMap.updated(bOO, aOO)
          warpZoneMap <- warpZoneMap.updated(bOO, aOO)
        } yield newWorld.copy(
          warpZoneMap = warpZoneMap, visibilityMap = visibilityMap
        )
      case _ => Evented(newWorld)
    }
  }
  def updated[A <: WObject](before: A, after: Option[A]): Evented[World] =
    after.fold(remove(before))(a => updated(before, a))
  def updated[A <: WObject](before: A)(afterFn: A => A): Evented[World] =
    updated(before, afterFn(before))
  private def updated(objects: WorldObjs): World = copy(objects = objects)
  private def updated(resources: Map[Player, Resources]): World =
    copy(resourcesMap = resources)

  def updateAll(pf: PartialFunction[WObject, WObject]) = {
    val liftedPf = pf.lift
    objects.foldLeft(Evented(this)) { case (evtWorld, beforeObj) =>
      liftedPf(beforeObj).fold2(
        evtWorld,
        obj => evtWorld.flatMap(w => w.updated(beforeObj, obj))
      )
    }
  }

  def resources(player: Player) = resourcesMap.getOrElse(player, Resources(0))

  def addResources(player: Player, count: Resources): Either[String, Evented[World]] = {
    val curRes = resources(player)
    if (count < Resources(0) && curRes < -count)
      s"Had $curRes, wanted to subtract ${-count}!".left
    else {
      val newRes = resources(player) + count
      Evented(
        updated(resourcesMap updated(player, newRes)),
        player.asHuman.fold2(
          Vector.empty, h => Vector(ResourceChangeEvt(h.right, newRes))
        )
      ).right
    }
  }

  def subResources(player: Player, count: Resources): Either[String, Evented[World]] =
    addResources(player, -count)

  def canWarp(b: Bounds) = bounds.contains(b) && ! objects.exists(_.bounds.intersects(b))

  private[this] def isVisibleFor(owner: Owner, f: Bounds => Boolean) =
    objects.view.collect { case obj: OwnedObj if obj.owner.isFriendOf(owner) => obj }.
      exists { obj => f(obj.visibility) }

  def visibleBy(owner: Owner): World = copy(
    objects = objects.filter(o => isVisiblePartial(owner, o.bounds)),
    resourcesMap = resourcesMap.filter { case (player, _) => owner.isFriendOf(player) },
    warpZoneMap = warpZoneMap.filter(owner),
    visibilityMap = visibilityMap.filter(owner)
  )

  /* Is any part of the bounds visible to owner */
  def isVisiblePartial(owner: Owner, b: Bounds): Boolean =
    visibilityMap.isVisiblePartial(owner, b)
  /* Is all of the bounds visible to owner. */
  def isVisibleFull(owner: Owner, b: Bounds): Boolean =
    visibilityMap.isVisibleFull(owner, b)
  def isVisibleFor(owner: Owner, v: Vect2): Boolean =
    visibilityMap.isVisible(owner, v)
  def isValidForWarp(owner: Owner, v: Vect2): Boolean = warpZoneMap.isVisible(owner, v)

  def objectsIn(vects: Vector[Vect2]): WorldObjs = objects.filter(vects)

  def reactTo[A <: OwnedObj](obj: A): WObject.WorldObjOptUpdate[A] = {
    @tailrec def react(
      reactors: Iterable[ReactiveFighter], current: WObject.WorldObjOptUpdate[A]
    ): WObject.WorldObjOptUpdate[A] =
      if (reactors.isEmpty) current
      else current.value match {
        case (_, None) => current
        case (world, Some(currentObj)) =>
          val reaction = reactors.head.reactTo(currentObj, world)
          val newWorld = current.events ++: reaction.value
          if (! reaction.abortReacting) react(reactors.tail, newWorld)
          else newWorld
      }

    val reactors = objects.collect { case rFighter: ReactiveFighter => rFighter }
    react(reactors, Evented((this, Some(obj))))
  }

  def findObj(id: WObject.Id) = objects.find(_.id === id)
  def findObjE(id: WObject.Id) = findObj(id).toRight(s"Can't find obj $id in world")
  def findObj(position: Vect2) = objects.find(_.position === position)
  def findObjE(position: Vect2) =
    findObj(position).toRight(s"Can't find obj @ $position in world")
  def find[A <: WObject](predicate: PartialFunction[WObject, A]): Option[A] =
    objects.collectFirst(predicate)
  def contains(id: Id): Boolean = objects.exists(_.id === id)

  lazy val owners = objects.collect { case fo: OwnedObj => fo.owner }.toSet
  lazy val teams = owners.map(_.team)
  lazy val players = owners.collect { case p: Player => p }.toSet ++ resourcesMap.keySet
  lazy val humans = players.collect { case h: Human => h }.toSet
  lazy val bots = players.collect { case b: Bot => b }.toSet
}

object World {
  def revealObjects(team: Team, evtWorld: Evented[World]): Evented[World] = {
    val newVisiblePoints = evtWorld.events.collect {
      case evt: VisibilityChangeEvt if evt.team === team => evt.visible
    }.flatten
    evtWorld.flatMap { newWorld =>
      val newVisibleObjs = newWorld.objectsIn(newVisiblePoints)
      val newVisibleObjEvents = newVisibleObjs.map(ObjVisibleEvt(team, newWorld, _))
      Evented(newWorld, newVisibleObjEvents.toVector)
    }
  }

  private def xTurnX[A : ClassTag](
    filter: A => Boolean, f: A => World => Evented[World]
  )(world: Evented[World]) = world.value.objects.foldLeft(world) {
    case (w, o: A) if filter(o) => w.laterFlatMap(f(o))
    case (w, o) => w
  }

  private def gameTurnX(
    f: WObject => World => Evented[World]
  )(world: Evented[World]) =
    xTurnX[WObject](_ => true, f)(world)

  private def teamTurnX(
    team: Team
  )(f: OwnedObj => World => Evented[World])(world: Evented[World]) =
    xTurnX[OwnedObj](_.owner.team === team, f)(world)

  private def runAI(team: Team)(e: Evented[World])(implicit log: LoggingAdapter) = {
    val bots = e.value.bots.filter(_.team === team)
    bots.foldLeft(e) { case (fEvtWorld, bot) =>
      fEvtWorld.flatMap { fWorld => GrowingSpawnerAI.act(fWorld, bot) }
    }
  }

  private[this] def randomDirection = {
    def rDir = Random.nextInt(3) - 1
    val hDir = rDir
    val vDir = Stream.continually(rDir).filter(d => hDir =/= 0 || d =/= 0).head
    Vect2(hDir, vDir)
  }

  private[this] def bounds(objects: TraversableOnce[WObject]) =
    objects.map(_.bounds).reduce(_ join _)

  def create(
    playersTeam: Team, npcOwner: => Bot,
    spawnerOwner: => Bot,
    startingPoint: Vect2 = Vect2(0, 0),
    endDistance: TileDistance = TileDistance(30),
    branches: Range = 2 to 12,
    spawners: Int = 2,
    jumpDistance: Range = 3 to 6,
    blobSize: Range = 2 to 5,
    blobRichness: Range = 15 to 60,
    asteroidResources: Range = 25 to 50,
    directionChangeChance: Double = 0.2,
    branchChance: Double = 0.2,
    safeDistance: TileDistance = TileDistance(10),
    enemyResourcesAtMaxDistance: Resources = Wasp.cost * Resources(3),
    npcChances: WeightedIS[WeightedIS[EmptySpaceWarpableCompanion[_ <: Warpable]]] =
      IndexedSeq(
        IndexedSeq(Wasp -> 1) -> 3,
//        IndexedSeq(RayShip -> 1) -> 3,
//        IndexedSeq(Fortress -> 1) -> 3,
        IndexedSeq(Wasp -> 2, Fortress -> 1) -> 2,
//        IndexedSeq(Wasp -> 2, RayShip -> 1) -> 2,
        IndexedSeq(Fortress -> 2, Wasp -> 1) -> 2,
        IndexedSeq(Fortress -> 2, RayShip -> 1) -> 1,
//        IndexedSeq(RayShip -> 2, Wasp -> 1) -> 2,
//        IndexedSeq(RayShip -> 2, Fortress -> 1) -> 2,
        IndexedSeq(Wasp -> 1, RayShip -> 1, Fortress -> 1) -> 1
      )
  )(implicit log1: LoggingAdapter) = {
    val log = new PrefixedLoggingAdapter("World#create|", log1)
    val warpGate = WarpGate(startingPoint, playersTeam)
    var objects = WorldObjs(warpGate).right_!
    // Main branch is also a branch.
    var branchesLeft = branches.random + 1
    var spawnersLeft = spawners
    log.debug(s"Creating map. Branches: $branchesLeft")

    def pTaken(v: Vect2): Boolean = objects.contains(v)
    def bTaken(bounds: Bounds): Boolean = ! objects.isAllFree(bounds)

    def tryN[A](n: Int)(f: => Option[A]): Option[A] = {
      var a = Option.empty[A]
      var i = 0
      while (i < n && a.isEmpty) {
        a = f
        i += 1
      }
      a
    }

    def spawnBlob(bounds: Bounds, log: LoggingAdapter): Unit = {
      val npcPool = npcChances.weightedRandom.get
      val enemyResourcesNeeded = Resources(math.min(
        (
          bounds.center.tileDistance(startingPoint).value.toDouble *
          enemyResourcesAtMaxDistance.value / endDistance.value
        ).round.toInt,
        enemyResourcesAtMaxDistance.value
      ))

      var resourcesLeft = blobRichness.random
      // Subtract resources already in the bounds.
      resourcesLeft -= objects.collect {
        case a: Asteroid if bounds.contains(a.position) => a.resources
      }.sum.value

      var enemyResourcesInBounds = objects.view.
        filter(o => bounds.contains(o.position)).collect {
          case oo: Warpable if oo.owner.team != playersTeam => oo.companion.cost
        }.sum
      log.debug(
        s"Blob spawn | bounds: {}, resources: ${resourcesLeft
        }, enemy resources needed: ${enemyResourcesNeeded
        }, already placed: $enemyResourcesInBounds", bounds
      )

      for (objPos <- Random.shuffle(bounds.points)) {
        // Have more space around asteroids
        lazy val asteroidFreeSpace = {
          val rnd = 1 to 3
          Bounds(objPos, Vect2(rnd.random, rnd.random))
        }

        if (
          resourcesLeft >= asteroidResources.start &&
          ! bTaken(asteroidFreeSpace)
        ) {
          val resources = math.min(resourcesLeft, asteroidResources.random)
          resourcesLeft -= resources
          log.debug(s"asteroid @ {} with $resources res, left: $resourcesLeft", objPos)
          objects += Asteroid(objPos, Resources(resources))
        }
        else if (
          enemyResourcesInBounds < enemyResourcesNeeded &&
          warpGate.bounds.perimeter.map(_.tileDistance(objPos)).
            forall(_ > safeDistance) &&
          ! pTaken(objPos)
        ) {
          val enemyOpt = tryN(10) {
            npcPool.weightedRandom.
              filter(_.cost <= enemyResourcesNeeded - enemyResourcesInBounds)
          }
          enemyOpt.foreach { enemyWarpable =>
            objects += enemyWarpable.warp(npcOwner, objPos)
            enemyResourcesInBounds += enemyWarpable.cost
          }
          log.debug(
            s"{} @ {}, left: ${enemyResourcesNeeded - enemyResourcesInBounds}",
            enemyOpt, objPos
          )
        }
      }

      log.debug("Blob spawn end")
    }

    def branch(branchStart: Vect2): Unit = {
      val branchId = branchesLeft
      val branchLog = log.prefixed(s"b$branchId|")
      branchLog.debug("Creating at {}", branchStart)
      branchesLeft -= 1
      var position = branchStart
      var direction = randomDirection

      do {
        val (hSize, vSize) = (blobSize.random, blobSize.random)
        val bounds = Bounds(
          (-hSize / 2) to (hSize / 2), (-vSize / 2) to (vSize / 2)
        ) + position

        spawnBlob(bounds, log)

        if (branchesLeft > 0 && Random.nextDouble() <= branchChance) {
          branchLog.debug(s"Branching off at {}", position)
          branch(position)
        }

        if (Random.nextDouble() <= directionChangeChance) direction = randomDirection
        position += direction * jumpDistance.random
        branchLog.debug(s"jump to {}", position)
      } while (position.tileDistance(startingPoint) < endDistance)

      if (spawnersLeft > 0) {
        var spawnerPos = position
        while (bTaken(Bounds(spawnerPos, Spawner.size))) spawnerPos += direction
        branchLog.debug(s"Spawner @ {}", spawnerPos)
        objects += Spawner(spawnerPos, spawnerOwner)
        spawnersLeft -= 1
      }

      branchLog.debug("end of branch")
    }

    (1 to 3).foreach { _ => spawnBlob(warpGate.visibility, log) }
    while (branchesLeft > 0) branch(warpGate.bounds.center)

    val bounds = this.bounds(objects) expandBy 5
    new World(
      bounds, objects, Map.empty,
      WarpZoneMap(bounds, objects), VisibilityMap(bounds, objects)
    )
  }
}