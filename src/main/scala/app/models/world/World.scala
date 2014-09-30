package app.models.world

import app.models.game.ai.GrowingSpawnerAI
import app.models.game.events.Evented
import app.models.world.WObject.Id
import app.models.world.buildings.{Spawner, WarpGate}
import app.models.world.props.Asteroid
import app.models.world.units.Wasp
import app.models._
import implicits._
import infrastructure.Log

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Random

case class World(
  bounds: Bounds, objects: Set[WObject]
) extends TurnBased[World] {
  import World._

  def gameTurnStarted = this |> gameTurnX(_.gameTurnStarted)
  def gameTurnFinished = this |> gameTurnX(_.gameTurnFinished)
  def teamTurnStarted(team: Team) =
    this |> teamTurnX(team)(_.teamTurnStarted) |> runAI(team)
  def teamTurnFinished(team: Team) =
    this |> teamTurnX(team)(_.teamTurnFinished)

  def actionsFor(player: Player) = objects.collect {
    case obj: GivingActions if obj.owner.team == player.team =>
      obj.companion.actionsGiven
  }.sum

  def add(obj: WObject) = {
    // DEBUG CHECK
    objects.find(o => obj.bounds.intersects(o.bounds)).foreach { o =>
      throw new Exception(s"Adding $obj to world - intersection with $o")
    }
    copy(objects = objects + obj)
  }
  def remove(obj: WObject) = copy(objects = objects - obj)
  def updated[A <: WObject](before: A, after: A): World = remove(before).add(after)
  def updated[A <: WObject](before: A, after: Option[A]): World = {
    val w = remove(before)
    after.fold(w)(w.add)
  }
  def updated[A <: WObject](before: A)(afterFn: A => A): World = updated(before, afterFn(before))

  def updateAll(pf: PartialFunction[WObject, WObject]) = {
    val lpf = pf.lift
    objects.foldLeft(this) { case (world, a) =>
      lpf(a).fold(world)(world.updated(a, _))
    }
  }

  def isFree(b: Bounds) = bounds.contains(b)

  private[this] def isVisibleFor(owner: Owner, f: Bounds => Boolean) =
    objects.view.collect { case obj: OwnedObj if obj.owner.isFriendOf(owner) => obj }.
      exists { obj => f(obj.visibility) }

  /* Is any part of the bounds visible to owner */
  def isVisiblePartial(owner: Owner, b: Bounds): Boolean =
    isVisibleFor(owner, _.intersects(b))
  /* Is all of the bounds visible to owner. */
  def isVisibleFull(owner: Owner, b: Bounds): Boolean =
    b.points.forall(isVisibleFor(owner, _)) /* TODO: optimize me */
  def isVisibleFor(owner: Owner, v: Vect2): Boolean =
    isVisibleFor(owner, _.contains(v))

  def reactTo[A <: OwnedObj](obj: A): WObject.WorldObjOptUpdate[A] = {
    @tailrec def react(
      reactors: Iterable[ReactiveFighter], current: WObject.WorldObjOptUpdate[A]
    ): WObject.WorldObjOptUpdate[A] =
      if (reactors.isEmpty) current
      else current.value match {
        case (_, None) => current
        case (world, Some(currentObj)) =>
          val reaction = reactors.head.reactTo(currentObj, world)
          val newWorld = current.events +: reaction.value
          if (! reaction.abortReacting) react(reactors.tail, newWorld)
          else newWorld
      }

    val reactors = objects.collect { case rFighter: ReactiveFighter => rFighter }
    react(reactors, Evented((this, Some(obj))))
  }

  def findObj(position: Vect2) = objects.find(_.position == position)
  def find[A <: WObject](predicate: PartialFunction[WObject, A]): Option[A] =
    objects.collectFirst(predicate)
  def contains(id: Id): Boolean = objects.exists(_.id == id)

  lazy val owners = objects.collect { case fo: OwnedObj => fo.owner }
  lazy val teams = owners.map(_.team)
  lazy val players = owners.collect { case p: Player => p }.toSet
  lazy val humans = players.collect { case h: Human => h }.toSet
  lazy val bots = players.collect { case b: Bot => b }.toSet
}

object World {
  private def xTurnX[A : ClassTag](
    filter: A => Boolean, f: A => World => Evented[World]
    )(world: World) = world.objects.foldLeft(Evented(world)) {
    case (w, o: A) if filter(o) => w.laterFlatMap(f(o))
    case (w, o) => w
  }

  private def gameTurnX(
    f: WObject => World => Evented[World]
    )(world: World) =
    xTurnX[WObject](_ => true, f)(world)

  private def teamTurnX(
    team: Team
    )(f: OwnedObj => World => Evented[World])(world: World) =
    xTurnX[OwnedObj](_.owner.team == team, f)(world)

  private def runAI(team: Team)(e: Evented[World]) = {
    val bots = e.value.bots.filter(_.team == team)
    bots.foldLeft(e) { case (fEvtWorld, bot) =>
      fEvtWorld.flatMap { fWorld => GrowingSpawnerAI.act(fWorld, bot) }
    }
  }

  private[this] def randomDirection = {
    def rDir = Random.nextInt(3) - 1
    val hDir = rDir
    val vDir = Stream.continually(rDir).filter(d => hDir != 0 || d != 0).head
    Vect2(hDir, vDir)
  }

  private[this] def bounds(objects: Set[WObject]) =
    objects.map(_.bounds).reduce(_ join _)

  def create(
    playersTeam: Team, waspsOwner: => Bot,
    spawnerOwner: => Bot,
    startingPoint: Vect2 = Vect2(0, 0),
    endDistance: TileDistance = TileDistance(30),
    branches: Range = 2 to 12,
    spawners: Int = 2,
    jumpDistance: Range = 3 to 6,
    blobSize: Range = 2 to 5,
    blobRichness: Range = 15 to 35,
    asteroidResources: Range = 5 to 20,
    directionChangeChance: Double = 0.2,
    branchChance: Double = 0.2,
    safeDistance: TileDistance = TileDistance(WarpGate.visibility + Wasp.visibility),
    waspsAtMaxDistance: Int = 3
  ) = {
    val warpGate = WarpGate(startingPoint, playersTeam)
    var objects = Set.apply[WObject](warpGate)
    // Main branch is also a branch.
    var branchesLeft = branches.random + 1
    var spawnersLeft = spawners
    Log.debug(s"Creating map. Branches: $branchesLeft")

    def pTaken(v: Vect2): Boolean = objects.exists(_.bounds.contains(v))
    def bTaken(bounds: Bounds): Boolean =
      objects.exists(_.bounds.intersects(bounds))

    def spawnBlob(bounds: Bounds, log: (=> String) => Unit): Unit = {
      val waspsNeeded = math.min(
        (
          bounds.center.tileDistance(startingPoint).value.toDouble *
          waspsAtMaxDistance / endDistance.value
        ).round.toInt,
        waspsAtMaxDistance
      )
      var resourcesLeft = blobRichness.random
      var waspsInBounds =
        objects.count(o => o.isInstanceOf[Wasp] && bounds.contains(o.position))
      log(
        s"Blob spawn | bounds: $bounds, resources: $resourcesLeft, " +
          s"wasps: $waspsNeeded, already placed: $waspsInBounds"
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
          log(s"asteroid @ $objPos with $resources res, left: $resourcesLeft")
          objects += Asteroid(objPos, resources)
        }
        else if (
          waspsInBounds < waspsNeeded &&
          warpGate.bounds.perimeter.map(_.tileDistance(objPos)).
            forall(_ > safeDistance) &&
          ! pTaken(objPos)
        ) {
          waspsInBounds += 1
          log(s"wasp @ $objPos, left: ${waspsNeeded - waspsInBounds}")
          objects += Wasp(objPos, waspsOwner)
        }
      }

      log("Blob spawn end")
    }

    def branch(branchStart: Vect2): Unit = {
      val branchId = branchesLeft
      def log(s: => String) = Log.debug(s"Branch $branchId: $s")
      log(s"Creating at $branchStart")
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
          log(s"Branching off at $position")
          branch(position)
        }

        if (Random.nextDouble() <= directionChangeChance) direction = randomDirection
        position += direction * jumpDistance.random
        log(s"jump to $position")
      } while (position.tileDistance(startingPoint) < endDistance)

      if (spawnersLeft > 0) {
        var spawnerPos = position
        while (bTaken(Bounds(spawnerPos, Spawner.size))) spawnerPos += direction
        log(s"Spawner @ $spawnerPos")
        objects += Spawner(spawnerPos, spawnerOwner)
        spawnersLeft -= 1
      }

      log("end of branch")
    }

    (1 to 3).foreach { _ => spawnBlob(warpGate.visibility, Log.debug) }
    while (branchesLeft > 0) branch(warpGate.bounds.center)

    new World(bounds(objects) expandBy 5, objects)
  }
}