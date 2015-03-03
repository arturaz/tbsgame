package app.models.game.world

import akka.event.LoggingAdapter
import app.models.game._
import app.models.game.ai.GrowingSpawnerAI
import app.models.game.events._
import app.models.game.world.WObject.Id
import app.models.game.world.buildings.{VPTowerStats, SpawnerStats, WarpGateStats}
import app.models.game.world.maps.{WasVisibleMap, VisibilityMap, WarpZoneMap}
import app.models.game.world.units.{FortressStats, RayShipStats, WaspStats}
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
  bounds: Bounds, objects: WorldObjs.All,
  wasVisibleMap: WasVisibleMap,
  playerStates: Map[Player, WorldPlayerState], vpsMap: Map[Team, VPS],
  warpZoneMap: WarpZoneMap, visibilityMap: VisibilityMap
) {
  import app.models.game.world.World._

  val resourcesMap = playerStates.mapValues(_.resources)
  override def toString = s"World($bounds, objects: ${objects.size})"

  def gameTurnStarted(implicit log: LoggingAdapter) =
    Evented(this) |>
    gameTurnX(log.prefixed("started|")) {
      implicit log => obj => world => WObject.gameTurnStarted(world, obj).map(_._1)
    }
  def gameTurnFinished(implicit log: LoggingAdapter) =
    Evented(this) |>
    gameTurnX(log.prefixed("finished|")) {
      implicit log => obj => world => WObject.gameTurnFinished(world, obj).map(_._1)
    }

  /* If team does not have any objects, there is nothing it can do thus it should be
     skipped. For example this can happen when playing 1v1 pvp and all NPC objects are
     eradicated. */
  def ifTeamExists(team: Team)(f: => Evented[World]) =
    if (teams.contains(team)) f else Evented(this)

  def teamTurnStarted(team: Team)(implicit log: LoggingAdapter) = ifTeamExists(team) {
    /* Run only if this team has objects */
    Evented(this, Vector(TurnStartedEvt(team))) |>
    teamTurnX(team, log.prefixed("started|")) {
      implicit log => obj => world => OwnedObj.teamTurnStarted(obj, world).map(_._1)
    } |>
    runAI(team)
  }
  def teamTurnFinished(team: Team)(implicit log: LoggingAdapter) = ifTeamExists(team) {
    Evented(this, Vector(TurnEndedEvt(team))) |>
    teamTurnX(team, log.prefixed("finished|")) {
      implicit log => obj => world => OwnedObj.teamTurnFinished(obj, world).map(_._1)
    }
  }

  def actionsFor(player: Player): Actions = {
    val actions = objects.collect {
      case obj: GivingActions if obj.owner.isFriendOf(player) =>
        obj.stats.actionsGiven
    }
    val total = actions.sum
    total
  }

  private[this] def changeWithMapUpdates[A, MapUpdateData](
    aToMapUpdateData: A => Option[MapUpdateData]
  )(a: A)(
    recalculatePopulationsFor: A => TraversableOnce[Owner],
    updateWorldObjects: (WorldObjs.All, A) => WorldObjs.All,
    updateWarpZone: (World, WarpZoneMap, MapUpdateData) => Evented[WarpZoneMap],
    updateVisibilityMap: (World, VisibilityMap, MapUpdateData) => Evented[VisibilityMap]
  ): Evented[World] = {
    val updatedWorld = copy(objects = updateWorldObjects(objects, a))
    val evtWithUpdatedMaps = aToMapUpdateData(a).fold2(
      Evented(updatedWorld),
      b => {
        val visibilityMapEvt = updateVisibilityMap(updatedWorld, visibilityMap, b)
        val wasVisibleMapEvt = updateWasVisibleMap(
          updatedWorld.objects, updatedWorld.wasVisibleMap, visibilityMapEvt
        )
        for {
          visibilityMap <- visibilityMapEvt
          wasVisibleMap <- wasVisibleMapEvt
          warpZoneMap <- updateWarpZone(updatedWorld, warpZoneMap, b)
        } yield updatedWorld.copy(
          warpZoneMap = warpZoneMap,
          visibilityMap = visibilityMap,
          wasVisibleMap = wasVisibleMap
        )
      }
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
    changeWithMapUpdates((obj: WObject) => obj.cast[OwnedObj]) _

  private[this] def objToOwner(obj: WObject) = obj.cast[OwnedObj].map(_.owner)

  def add(obj: WObject): Evented[World] =
    changeWithMapUpdatesSingle(obj)(
      objToOwner,
      _ + _,
      (newWorld, m, o) => m + (o, objects),
      (newWorld, m, o) => m + (o, objects)
    )
  def remove(obj: WObject): Evented[World] =
    changeWithMapUpdatesSingle(obj)(
      objToOwner,
      _ - _.id,
      (newWorld, m, o) => m - (o, objects),
      (newWorld, m, o) => m - (o, objects)
    )
  def removeEvt(obj: WObject): Evented[World] =
    Evented(this, RealObjDestroyedEvt(visibilityMap, obj)).flatMap(_.remove(obj))

  def updated[A <: WObject](before: A, after: A): Evented[World] = {
    changeWithMapUpdates[(A, A), (OwnedObj, OwnedObj)] {
      case (before, after) => (before.cast[OwnedObj], after.cast[OwnedObj]) match {
        case (Some(bOO), Some(aOO)) => Some((bOO, aOO))
        case _ => None
      }
    }(before, after)(
      { case (before, after) => objToOwner(before).toSet ++ objToOwner(after).toSet },
      { case (objs, (before, after)) => objs update_! (before, after) },
      { case (newWorld, warpZoneMap, (bOO, aOO)) =>
        warpZoneMap updated ((objects, bOO), (newWorld.objects, aOO))
      },
      { case (newWorld, visibilityMap, (bOO, aOO)) =>
        visibilityMap updated ((objects, bOO), (newWorld.objects, aOO))
      }
    )
  }
  def updated[A <: WObject](before: A, after: Option[A]): Evented[World] =
    after.fold(remove(before))(a => updated(before, a))
  def updated[A <: WObject](before: A)(afterFn: A => A): Evented[World] =
    updated(before, afterFn(before))
  private def updated(objects: WorldObjs.All): World = copy(objects = objects)
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
      state.withValue(_ + oo.stats.populationCost)
    case (state, _) =>
      state
  }

  def vps(owner: Owner) = vpsMap.getOrElse(owner.team, VPS(0))

  def addVps(owner: Owner, count: VPS): World =
    updatedVps(vpsMap updated (owner.team, vps(owner) + count))

  def canWarp(b: Bounds) = bounds.contains(b) && ! objects.exists(_.bounds.intersects(b))

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

  def noLongerVisibleImmovableObjectsFor(team: Team) =
    wasVisibleMap.getOrElse(team, WorldObjs.empty)

  def reactTo[A <: OwnedObj](obj: A)(implicit log: LoggingAdapter)
  : WObject.WorldObjOptUpdate[A] = {
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
      val newVisibleObjs = world.objects.filterPartial(newVisiblePoints)
      val newVisibleObjEvents = newVisibleObjs.map(ObjVisibleEvt(team, _))
      Evented(world, newVisibleObjEvents.toVector)
    }
  }

  private def updateWasVisibleMap(
    worldObjs: WorldObjs.All, previousWasVisibleMap: WasVisibleMap,
    visibilityMapEvt: Evented[VisibilityMap]
  ): Evented[WasVisibleMap] = {
    val visibilityMap = visibilityMapEvt.value
    val events = visibilityMapEvt.events.collect { case e: VisibilityChangeEvt => e }
    events.foldLeft(Evented(previousWasVisibleMap)) { case (wvMapEvt, evt) =>
      wvMapEvt.flatMap { wvMap =>
        val wasVisible = wvMap.getOrElse(evt.team, WorldObjs.empty)

        val previouslyCachedObjs = wasVisible.objectsIn(evt.visible)
        // Objects that existed when the zone was visible but do not exist no more.
        val destroyInClient =
          previouslyCachedObjs.filterNot(o => worldObjs.contains(o.id))

        // Points that became invisible - add all world objects that are in those points
        // to the cache.
        val fullyInvisibleObjects =
          worldObjs.objectsIn(evt.invisible)
            .collect {
              case WObject.Static(o)
                // Ensure that objects are fully invisible.
                if !visibilityMap.isVisiblePartial(evt.team, o.bounds) => o
            }

        // Then for all points that became visible - remove all world objects that are in
        // those points from the cache.
        val visibleObjectIds =
          worldObjs.objectsIn(evt.visible)
            .collect { case WObject.Static(o) if wasVisible.contains(o.id) => o.id }

        val newWasVisible = wasVisible ++ fullyInvisibleObjects -- visibleObjectIds

        Evented(
          wvMap updated (evt.team, newWasVisible),
          destroyInClient.map(GhostObjDestroyedEvt(evt.team, _)).toVector
        )
      }
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

  private[this] def randomDirectionAll(includeDiagonals: Boolean) = {
    def rDir = Random.nextInt(3) - 1
    val hDir = rDir
    val vDir =
      if (includeDiagonals)
        Stream.continually(rDir).filter(d => hDir =/= 0 || d =/= 0).head
      else
        if (hDir === 0) rDir else 0
    Vect2(hDir, vDir)
  }

  private[this] def randomDirection = randomDirectionAll(true)
  private[this] def randomHVDirection = randomDirectionAll(false)

  private[this] def bounds(objects: TraversableOnce[WObject]) =
    objects.map(_.bounds).reduce(_ join _)

  def create(
    playersTeam: Team, npcOwner: () => Bot, spawnerOwner: () => Bot,
    staticObjectsKnownAtStart: Boolean,
    startingPoint: Vect2 = Vect2(0, 0),
    endDistance: TileDistance = TileDistance(30),
    branches: Range = 4 to 6,
    rockLines: Range = 0 to 4,
    rockLineLength: Range = 1 to 4,
    rockLineDirectionChangeChance: Double = 0.2,
    spawners: Int = 2,
    jumpDistance: Range = 3 to 6,
    blobSize: Range = 2 to 5,
    blobRichness: Range = 25 to 60,
    asteroidResources: Range = 25 to 50,
    directionChangeChance: Double = 0.2,
    branchChance: Double = 0.4,
    safeDistance: TileDistance = TileDistance(10),
    enemyResourcesAtMaxDistance: Resources = WaspStats.cost * Resources(2),
    vpTowers: Int = 3
  )(implicit initialLog: LoggingAdapter) = {
    val npcChances = IndexedSeq(
      IndexedSeq(WaspStats -> 1) -> 3,
      IndexedSeq(WaspStats -> 2, FortressStats -> 1) -> 2,
      IndexedSeq(FortressStats -> 2, WaspStats -> 1) -> 2,
      IndexedSeq(FortressStats -> 2, RayShipStats -> 1) -> 1,
      IndexedSeq(WaspStats -> 1, RayShipStats -> 1, FortressStats -> 1) -> 1
    )

    val log = new PrefixedLoggingAdapter("World#create|", initialLog)
    val warpGate = WarpGate(startingPoint, playersTeam)
    var objects = WorldObjs[WObject](warpGate).right_!
    // Main branch is also a branch.
    var branchesLeft = branches.random + 1
    var spawnersLeft = spawners
    log.debug(s"Creating map. Branches: $branchesLeft")

    def pTaken(v: Vect2): Boolean = objects.nonEmptyAt(v)
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
          case oo: Warpable if oo.owner.team != playersTeam => oo.stats.cost
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
          objects += Asteroid(objPos, Resources(resources), Resources(1))
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
            objects += enemyWarpable.warp(npcOwner(), objPos)
            enemyResourcesInBounds += enemyWarpable.cost
          }
          log.debug(
            s"{} @ {}, left: ${enemyResourcesNeeded - enemyResourcesInBounds}",
            enemyOpt, objPos
          )
        }
      }

      spawnRocks(bounds)

      log.debug("Blob spawn end")
    }

    def spawnRocks(bounds: Bounds): Unit = {
      val shuffledPoints = Random.shuffle(bounds.points)

      (1 to rockLines.random).foreach { _ =>
        val startOpt =
          shuffledPoints.collectFirst { case p if objects.nonEmptyAt(p) => p }
        startOpt.foreach { start =>
          var position = start
          val length = rockLineLength.random
          var placed = 0
          var direction = randomHVDirection

          while (bounds.contains(position) && placed < length) {
            if (objects.emptyAt(position)) {
              objects += Rock(position)
              placed += 1
            }

            if (Random.chance(rockLineDirectionChangeChance))
              direction = randomHVDirection
            position += direction
          }
        }
      }
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
        while (bTaken(Bounds(spawnerPos, SpawnerStats.size))) spawnerPos += direction
        val spawner = Spawner(spawnerPos, spawnerOwner())
        objects += spawner
        branchLog.debug(s"Spawner @ {}: {}", spawnerPos, spawner)
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
      if (! bTaken(Bounds(position, VPTowerStats.size))) {
        val vpTower = VPTower(position, spawnerOwner().team)
        objects += vpTower
        log.debug("vp tower @ {}: {}", position, vpTower)
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
    World(bounds, objects, staticObjectsKnownAtStart)
  }

  def apply(
    bounds: Bounds, objects: WorldObjs.All, staticObjectsKnownAtStart: Boolean
  ) = {
    val world = new World(
      bounds, objects, Map.empty, Map.empty, Map.empty,
      WarpZoneMap(bounds, objects), VisibilityMap(bounds, objects)
    )
    if (staticObjectsKnownAtStart) {
      val statics = world.objects.collectWO { case WObject.Static(o) => o }
      val wasVisible = world.teams.map { team =>
        team -> statics.filterNot { o =>
          o.cast[OwnedObj].exists(oo =>
            oo.stats.isCritical && oo.owner.isEnemyOf(team)
          ) || world.isVisiblePartial(team, o.bounds)
        }
      }.toMap
      world.copy(wasVisibleMap = wasVisible)
    }
    else world
  }
}