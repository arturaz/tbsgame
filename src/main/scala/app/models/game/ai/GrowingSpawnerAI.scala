package app.models.game.ai

import akka.event.LoggingAdapter
import app.algorithms.Pathfinding
import app.models.game.{Actions, Owner}
import app.models.game.events.Evented
import app.models.game.world._
import implicits._

import scala.annotation.tailrec
import scala.util.Random

object GrowingSpawnerAI {
  type SpawnerUnit = WUnit with Fighter
  type ReadyUnits = List[SpawnerUnit]
  type Result = Evented[World]

  def act(world: World, owner: Owner)(implicit log: LoggingAdapter): Result = {
    val spawners = world.objects.collect {
      case spawner: Spawner if owner === (spawner.owner: Owner) => spawner
    }

    if (spawners.isEmpty)
      SingleMindAI.act(world, owner)
    else {
      val actions = world.objects.collect {
        case ga: GivingActions if owner.team == ga.owner.team => ga.stats.actionsGiven
      }.sum
      val readyUnits = world.objects.collect {
        case unit: SpawnerUnit
          if (unit.owner: Owner) === owner && (
            unit.movementLeft.isNotZero || unit.hasAttacksLeft
          ) && unit.isWarpedIn => unit
      }.toList

      spawners.foldLeft(Evented((world, readyUnits))) { case (fEvtWorld, spawner) =>
        fEvtWorld.flatMap { case (fWorld, fReadyUnits) =>
          val (newWorld, newReadyUnits) = act(
            fWorld, spawner, actions, fReadyUnits
          )(log.prefixed(s"GrowingSpawnerAI[$spawner]|"))
          newWorld.map((_, newReadyUnits))
        }
      }.map(_._1)
    }
  }

  def act(
    world: World, spawner: Spawner, actions: Actions, readyUnits: ReadyUnits
  )(implicit log: LoggingAdapter): (Result, ReadyUnits) = {
    @tailrec def work(
      actionsLeft: SpawnerStr, world: Evented[World], readyUnits: ReadyUnits
    ): (Result, ReadyUnits) = {
      val orig = (world, readyUnits)
      log.debug("acting with actions left={}", actionsLeft)
      if (actionsLeft.isZero) orig
      else if (actionsLeft <= SpawnerStr(0)) {
        log.error("actionsLeft < 0! {}", actionsLeft)
        orig
      }
      else {
        val newActions = actionsLeft - SpawnerStr(1)
        readyUnits match {
          case unit :: rest =>
            work(newActions, world.flatMap(act(_, unit)), rest)
          case Nil => spawn(world.value, spawner) match {
            case Left(err) =>
              log.error("error while spawning unit: {}", err)
              orig
            case Right(newWorld) =>
              log.debug("spawned {}, events: {}", newWorld.value._2, newWorld.events)
              val newReadyUnits =
                newWorld.value._2.filter(_.isWarpedIn).fold2(List.empty, List(_)) :::
                  readyUnits
              work(newActions, world.events ++: newWorld.map(_._1), newReadyUnits)
          }
        }
      }
    }

    work(spawner.strength(actions), Evented(world), readyUnits)
  }

  def act(
    world: World, unit: SpawnerUnit
  )(implicit log: LoggingAdapter): Result = {
    log.debug("unit {} acting", unit)
    var possibleTargets = Set.empty[OwnedObj]

    SingleMindAI.whileHasAttacksLeft(world, unit)(
      (world, unit) => {
        possibleTargets = world.objects.collect {
          case fo: OwnedObj if fo.isEnemy(unit) => fo
        }.toSet

        SingleMindAI.findAndMoveAttackTarget(world, possibleTargets, unit)
      },
      // If that fails move towards either nearest critical object or just nearest object
      (evtWorld, unit) => {
        def select(s: Iterable[OwnedObj]) = s.minBy(_.bounds.tileDistance(unit.position))
        val optNewWorld = for {
          target <- possibleTargets.filter(_.stats.isCritical) match {
            case s if s.isEmpty =>
              if (possibleTargets.isEmpty) None
              else Some(select(possibleTargets))
            case s => Some(select(s))
          }
          path <- Pathfinding.aStar(
            unit, target.bounds, world.bounds, world.objects
          )
        } yield unit.moveTo(world, path.limit(unit.movementLeft)).fold(
          err => {
            log.error("move act {}: {}", unit, err)
            Evented(world)
          },
          update => {
            log.debug("nothing to attack, {} moving towards {}", unit, target)
            update.map(_._1)
          }
        )
        optNewWorld.getOrElse(Evented(world)).right
      }
    ).fold(
      err => {
        log.error(s"act {}: {}", unit, err)
        Evented(world)
      },
      identity
    )
  }

  private[this] def spawn(
    world: World, spawner: Spawner
  ): Either[String, WObject.WorldObjOptUpdate[SpawnerUnit]] = {
    val warpZones = world.objects.collect {
      case o: OwnedObj if o.owner.team == spawner.owner.team => o.warpZone
    }.collect { case Some(wz) => wz }

    val points = Random.shuffle(warpZones.flatMap(_.points).toVector)
    points.view.map(spawner.spawn(world, _)).find(_.isRight).getOrElse(
      s"$spawner couldn't find a place to spawn a unit within $warpZones".left
    )
  }
}
