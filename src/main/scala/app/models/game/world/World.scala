package app.models.game.world

import akka.event.LoggingAdapter
import app.models.game._
import app.models.game.ai.GrowingSpawnerAI
import app.models.game.events._
import app.models.game.world.WObject.Id
import app.models.game.world.buildings.{VPTower, Spawner, WarpGate}
import app.models.game.world.maps.{VisibilityMap, WarpZoneMap}
import app.models.game.world.props.Asteroid
import app.models.game.world.units.{Fortress, RayShip, Wasp}
import implicits._
import infrastructure.PrefixedLoggingAdapter
import monocle.{Lenser, SimpleLens}
import utils.{ValWithMax, IntValueClass}

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Random

case class WorldPlayerState(resources: Resources)
object WorldPlayerState {
  val empty = WorldPlayerState(Resources(0))
  val lenser = Lenser[WorldPlayerState]
  val resources = lenser(_.resources)
}

case class World private (
  bounds: Bounds, objects: WorldObjs,
  playerStates: Map[Player, WorldPlayerState], vpsMap: Map[Team, VPS],
  warpZoneMap: WarpZoneMap, visibilityMap: VisibilityMap
) {
  import app.models.game.world.World._

  val resourcesMap = playerStates.mapValues(_.resources)
  override def toString = s"World($bounds, objects: ${objects.size})"

  def gameTurnStarted(implicit log: LoggingAdapter) =
    Evented(this) |>
    gameTurnX(log.prefixed("started|"))(implicit log => _.gameTurnStarted)
  def gameTurnFinished(implicit log: LoggingAdapter) =
    Evented(this) |>
    gameTurnX(log.prefixed("finished|"))(implicit log => _.gameTurnFinished)
  def teamTurnStarted(team: Team)(implicit log: LoggingAdapter) =
    Evented(this, Vector(TurnStartedEvt(team))) |>
    teamTurnX(team, log.prefixed("started|"))(implicit log => _.teamTurnStarted) |>
    runAI(team)
  def teamTurnFinished(team: Team)(implicit log: LoggingAdapter) =
    Evented(this, Vector(TurnEndedEvt(team))) |>
    teamTurnX(team, log.prefixed("finished|"))(implicit log => _.teamTurnFinished)

  def actionsFor(player: Player): Actions = {
    val actions = objects.collect {
      case obj: GivingActions if obj.owner.isFriendOf(player) =>
        obj.companion.actionsGiven
    }
    val total = actions.sum
    total
  }

  private[this] def changeWithMapUpdates[A, MapUpdateData](
    aToMapUpdateData: A => Option[MapUpdateData]
  )(a: A)(
    recalculatePopulationsFor: A => TraversableOnce[Owner],
    updateWorldObjects: (WorldObjs, A) => WorldObjs,
    updateWarpZone: (WarpZoneMap, MapUpdateData) => Evented[WarpZoneMap],
    updateVisibilityMap: (VisibilityMap, MapUpdateData) => Evented[VisibilityMap]
  ): Evented[World] = {
    val updatedWorld = copy(objects = updateWorldObjects(objects, a))
    val evtWithUpdatedMaps = aToMapUpdateData(a).fold2(
      Evented(updatedWorld),
      b => for {
        visibilityMap <- updateVisibilityMap(visibilityMap, b)
        warpZoneMap <- updateWarpZone(warpZoneMap, b)
      } yield updatedWorld.copy(
        warpZoneMap = warpZoneMap,
        visibilityMap = visibilityMap
      )
    )
    recalculatePopulationsFor(a).foldLeft(evtWithUpdatedMaps) { case (fEvtWorld, owner) =>
      fEvtWorld.flatMap { world => Evented(
        world,
        (owner match {
          case t: Team => world.playerStates.keys.filter(_.team === t).toVector
          case p: Player => Vector(p)
        }).flatMap { player =>
          val populationBefore = this.populationFor(player)
          val populationNow = world.populationFor(player)
          if (populationBefore === populationNow) Vector.empty
          else Vector(PopulationChangeEvt(player, populationNow))
        }
      ) }
    }
  }
  private[this] val changeWithMapUpdatesSingle =
    changeWithMapUpdates((obj: WObject) => obj.asOwnedObj) _

  private[this] def objToOwner(obj: WObject) = obj.cast[OwnedObj].map(_.owner)

  def add(obj: WObject): Evented[World] =
    changeWithMapUpdatesSingle(obj)(objToOwner, _ + _, _ + _, _ + _)
  def remove(obj: WObject): Evented[World] =
    changeWithMapUpdatesSingle(obj)(objToOwner, _ - _.id, _ - _, _ - _)
  def removeEvt(obj: WObject): Evented[World] =
    Evented(this, ObjDestroyedEvt(this, obj)).flatMap(_.remove(obj))

  def updated[A <: WObject](before: A, after: A): Evented[World] = {
    changeWithMapUpdates[(A, A), (OwnedObj, OwnedObj)] {
      case (before, after) => (before.asOwnedObj, after.asOwnedObj) match {
        case (Some(bOO), Some(aOO)) => Some((bOO, aOO))
        case _ => None
      }
    }(before, after)(
      { case (before, after) => objToOwner(before).toSet ++ objToOwner(after).toSet },
      { case (objs, (before, after)) => objs update_! (before, after) },
      { case (warpZoneMap, (bOO, aOO)) => warpZoneMap updated (bOO, aOO) },
      { case (visibilityMap, (bOO, aOO)) => visibilityMap updated (bOO, aOO) }
    )
  }
  def updated[A <: WObject](before: A, after: Option[A]): Evented[World] =
    after.fold(remove(before))(a => updated(before, a))
  def updated[A <: WObject](before: A)(afterFn: A => A): Evented[World] =
    updated(before, afterFn(before))
  private def updated(objects: WorldObjs): World = copy(objects = objects)
  private def updatedPlayerStates(states: Map[Player, WorldPlayerState]): World =
    copy(playerStates = states)
  private def updatedVps(vps: Map[Team, VPS]): World = copy(vpsMap = vps)

  def updateAll(pf: PartialFunction[WObject, WObject]) = {
    val liftedPf = pf.lift
    objects.foldLeft(Evented(this)) { case (evtWorld, beforeObj) =>
      liftedPf(beforeObj).fold2(
        evtWorld,
        obj => evtWorld.flatMap(w => w.updated(beforeObj, obj))
      )
    }
  }

  def state(player: Player) =
    playerStates.getOrElse(player, WorldPlayerState.empty)
  def resources(player: Player) = state(player).resources

  def addPlayerState[Res <: IntValueClass[Res]](
    player: Player, count: Res, lens: SimpleLens[WorldPlayerState, Res]
  )(events: Res => Vector[Event]): Either[String, Evented[World]] = {
    val curState = state(player)
    val curS = lens.get(curState)
    if (count.isNegative && curS < -count)
      s"Had $curS, wanted to subtract ${-count}!".left
    else {
      val newRes = curS + count
      val newState = lens.set(curState, newRes)
      Evented(
        updatedPlayerStates(playerStates updated (player, newState)),
        events(newRes)
      ).right
    }
  }

  def addResources(player: Player, count: Resources): Either[String, Evented[World]] =
    addPlayerState(player, count, WorldPlayerState.resources) { newRes =>
      player.asHuman.fold2(Vector.empty, h => Vector(ResourceChangeEvt(h.right, newRes)))
    }

  def subResources(player: Player, count: Resources): Either[String, Evented[World]] =
    addResources(player, -count)

  def populationFor(owner: Owner): ValWithMax[Population] = objects.foldLeft(
    ValWithMax(Population(0), Population(0))
  ) {
    case (state, gp: GivingPopulation) if gp.owner.isFriendOf(owner) =>
      state.withMax(_ + gp.populationGiven)
    case (state, oo: Warpable) if oo.owner === owner =>
      state.withValue(_ + oo.companion.populationCost)
    case (state, _) =>
      state
  }

  def vps(owner: Owner) = vpsMap.getOrElse(owner.team, VPS(0))

  def addVps(owner: Owner, count: VPS): World =
    updatedVps(vpsMap updated (owner.team, vps(owner) + count))

  def canWarp(b: Bounds) = bounds.contains(b) && ! objects.exists(_.bounds.intersects(b))

  private[this] def isVisibleFor(owner: Owner, f: Bounds => Boolean) =
    objects.view.collect { case obj: OwnedObj if obj.owner.isFriendOf(owner) => obj }.
      exists { obj => f(obj.visibility) }

  def visibleBy(owner: Owner): World = copy(
    objects = objects.filter(o => isVisiblePartial(owner, o.bounds)),
    playerStates = playerStates.filter { case (player, _) => owner.isFriendOf(player) },
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

  def objectsPartlyIn(vects: Vector[Vect2]): WorldObjs = objects.filterPartial(vects)

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
  lazy val players = owners.collect { case p: Player => p }.toSet ++ playerStates.keySet
  lazy val humans = players.collect { case h: Human => h }.toSet
  lazy val bots = players.collect { case b: Bot => b }.toSet
}

object World {
  def revealObjects(team: Team, evtWorld: Evented[World]): Evented[World] = {
    val newVisiblePoints = evtWorld.events.collect {
      case evt: VisibilityChangeEvt if evt.team === team => evt.visible
    }.flatten
    evtWorld.flatMap { world =>
      val newVisibleObjs = world.objectsPartlyIn(newVisiblePoints)
      val newVisibleObjEvents = newVisibleObjs.map(ObjVisibleEvt(team, world, _))
      Evented(world, newVisibleObjEvents.toVector)
    }
  }

  private def xTurnX[A : ClassTag](
    log: LoggingAdapter, filter: A => Boolean,
    f: LoggingAdapter => A => World => Evented[World]
  )(world: Evented[World]) = world.value.objects.foldLeft(world) {
    case (w, o: A) if filter(o) => w.laterFlatMap(f(log.prefixed(o.toString))(o))
    case (w, o) => w
  }

  private def gameTurnX(log: LoggingAdapter)(
    f: LoggingAdapter => WObject => World => Evented[World]
  )(world: Evented[World]) =
    xTurnX[WObject](log.prefixed("game|"), _ => true, f)(world)

  private def teamTurnX(team: Team, log: LoggingAdapter)(
    f: LoggingAdapter => OwnedObj => World => Evented[World]
  )(world: Evented[World]) =
    xTurnX[OwnedObj](log.prefixed("team|"), _.owner.team === team, f)(world)

  private def runAI(team: Team)(e: Evented[World])(implicit log: LoggingAdapter) = {
    val scopedLog = log.prefixed("runAI|")
    val bots = e.value.bots.filter(_.team === team)
    bots.foldLeft(e) { case (fEvtWorld, bot) =>
      fEvtWorld.flatMap { fWorld => GrowingSpawnerAI.act(fWorld, bot)(scopedLog) }
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
      ),
    vpTowers: Int = 3
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

    var vpTowersLeft = vpTowers
    val vpDistance = endDistance / TileDistance(2)
    while (vpTowersLeft > 0) {
      val angle = Random.double(0, Math.PI)
      val position = Vect2(
        (vpDistance.value * Math.cos(angle)).round.toInt,
        (vpDistance.value * Math.sin(angle)).round.toInt
      ) + warpGate.bounds.center
      if (! bTaken(Bounds(position, VPTower.size))) {
        objects += VPTower(position, spawnerOwner.team)
        vpTowersLeft -= 1
      }
      else {
        log.debug(
          "vp tower position {} is taken, angle={}, vpDistance={}, left={}",
          position, angle, vpDistance, vpTowersLeft
        )
      }
    }

    val bounds = this.bounds(objects) expandBy 5
    new World(
      bounds, objects, Map.empty, Map.empty,
      WarpZoneMap(bounds, objects), VisibilityMap(bounds, objects)
    )
  }
}