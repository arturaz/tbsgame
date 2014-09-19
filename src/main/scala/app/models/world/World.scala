package app.models.world

import app.models.world.buildings.{Spawner, WarpGate}
import app.models.world.props.Asteroid
import app.models.world.units.Wasp
import app.models.{Attack, Player, Team}
import implicits._
import infrastructure.Log

import scala.util.Random

case class World(bounds: Bounds, objects: Set[WObject]) {
  def nextTurn: World = copy(objects = objects.map(_.nextTurn))

  def add(obj: WObject) = {
    // DEBUG CHECK
    objects.find(o => obj.bounds.intersects(o.bounds)).foreach { o =>
      throw new Exception(s"Adding $obj to world - intersection with $o")
    }
    copy(objects = objects + obj)
  }
  def remove(obj: WObject) = copy(objects = objects - obj)
  def update[A <: WObject](before: A, after: A) = remove(before).add(after)

  def update(attack: Attack, target: FactionObj): World =
    if (attack.successful)
      target.takeDamage.fold(remove(target))(update(target, _))
    else
      this

  def updateAll(pf: PartialFunction[WObject, WObject]) = {
    val lpf = pf.lift
    objects.foldLeft(this) { case (world, a) =>
      lpf(a).fold(world)(world.update(a, _))
    }
  }

  lazy val owners = objects.collect { case fo: FactionObj => fo.owner }
  lazy val teams = owners.map(_.team)
  lazy val players = owners.collect { case p: Player => p }.toSet
}

object World {
  private[this] def randomDirection = {
    def rDir = Random.nextInt(3) - 1
    val hDir = rDir
    val vDir = Stream.continually(rDir).filter(d => hDir != 0 || d != 0).head
    Vect2(hDir, vDir)
  }

  private[this] def bounds(objects: Set[WObject]) =
    objects.map(_.bounds).reduce(_ join _)

  def create(
    playersTeam: Team, waspsOwner: () => Player,
    spawnerOwner: () => Player,
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
          objects += Wasp(objPos, waspsOwner())
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
        objects += Spawner(spawnerPos, spawnerOwner())
        spawnersLeft -= 1
      }

      log("end of branch")
    }

    (1 to 3).foreach { _ => spawnBlob(warpGate.visibility, Log.debug) }
    while (branchesLeft > 0) branch(warpGate.bounds.center)

    new World(bounds(objects) expandBy 5, objects)
  }
}