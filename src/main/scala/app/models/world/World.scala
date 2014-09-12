package app.models.world

import app.models.Team
import app.models.world.buildings.{Spawner, WarpGate}
import app.models.world.props.Asteroid
import app.models.world.units.Wasp
import implicits._
import infrastructure.Log

import scala.util.Random

class World(var objects: Set[WObject]) {
  val bounds = {
    val xMin = objects.view.map(_.bounds).minBy(_.x.start).x.start
    val xMax = objects.view.map(_.bounds).maxBy(_.x.end).x.end
    val yMin = objects.view.map(_.bounds).minBy(_.y.start).y.start
    val yMax = objects.view.map(_.bounds).maxBy(_.y.end).y.end
    Bounds(xMin to xMax, yMin to yMax)
  }
}

object World {
  def randomDirection = {
    def rDir = Random.nextInt(3) - 1
    val hDir = rDir
    val vDir = Stream.continually(rDir).filter(d => hDir != 0 || d != 0).head
    Vect2(hDir, vDir)
  }

  def create(
    startingPoint: Vect2 = Vect2(0, 0),
    endDistance: Int = 30,
    branches: Range = 2 to 12,
    spawners: Int = 2,
    jumpDistance: Range = 3 to 6,
    blobSize: Range = 2 to 5,
    blobRichness: Range = 15 to 35,
    asteroidResources: Range = 5 to 20,
    directionChangeChance: Double = 0.2,
    branchChance: Double = 0.2,
    safeDistance: Int = WarpGate.visibility + Wasp.visibility,
    waspsAtMaxDistance: Int = 3
  ) = {
    val playerTeam = new Team
    val aiTeam = new Team

    val warpGate = new WarpGate(startingPoint, playerTeam)
    var objects = Set.apply[WObject](warpGate)
    // Main branch is also a branch.
    var branchesLeft = branches.random + 1
    var spawnersLeft = spawners
    Log.debug(s"Creating map. Branches: $branchesLeft")

    def taken(bounds: Bounds) = objects.exists(_.bounds.intersects(bounds))

    def spawnBlob(bounds: Bounds, log: (=> String) => Unit): Unit = {
      val waspsNeeded = math.min(
        (
          bounds.center.tileDistance(startingPoint).toDouble *
          waspsAtMaxDistance / endDistance
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

      def continueAsteroids = resourcesLeft >= asteroidResources.start
      def continueWasps = waspsInBounds < waspsNeeded
      def continue = continueAsteroids || continueWasps

      (0 to 10).takeWhile(_ => continue).foreach { idx =>
        log(s"blob iteration $idx")
        for (objPos <- bounds.points) {
          if (
            continueAsteroids &&
            Random.nextDouble() <= 0.25 &&
            !taken(Bounds(objPos, Asteroid.size))
          ) {
            val resources = math.min(resourcesLeft, asteroidResources.random)
            resourcesLeft -= resources
            log(s"asteroid @ $objPos with $resources res, left: $resourcesLeft")
            objects += new Asteroid(objPos, resources)
          }
          else if (
            continueWasps &&
            Random.nextDouble() <= 0.15 &&
            warpGate.bounds.perimeter.map(_.tileDistance(objPos)).
              forall(_ > safeDistance) &&
            !taken(Bounds(objPos, Wasp.size))
          ) {
            waspsInBounds += 1
            log(s"wasp @ $objPos, left: ${waspsNeeded - waspsInBounds}")
            objects += new Wasp(objPos, aiTeam)
          }
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
        while (taken(Bounds(spawnerPos, Spawner.size))) spawnerPos += direction
        log(s"Spawner @ $spawnerPos")
        objects += new Spawner(spawnerPos, aiTeam)
        spawnersLeft -= 1
      }

      log("end of branch")
    }

    (1 to 3).foreach { _ => spawnBlob(warpGate.visibility, Log.debug) }
    while (branchesLeft > 0) branch(warpGate.bounds.center)

    new World(objects)
  }
}