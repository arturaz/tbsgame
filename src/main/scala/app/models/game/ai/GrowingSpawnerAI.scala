package app.models.game.ai

import akka.event.LoggingAdapter
import app.algorithms.Pathfinding
import app.models.game.{Actions, Owner}
import app.models.game.events.Evented
import app.models.game.world.buildings.GrowingSpawner
import app.models.game.world.{SpawnerStr, OwnedObj, WObject, World}
import implicits._

import scala.annotation.tailrec
import scala.util.Random

object GrowingSpawnerAI {
  type Result = Evented[World]

  def act(world: World, owner: Owner)(implicit log: LoggingAdapter): Result = {
    val spawners = world.objects.collect {
      case spawner: GrowingSpawner if owner === (spawner.owner: Owner) => spawner
    }

    if (spawners.isEmpty)
      SingleMindAI.act(world, owner)
    else
      spawners.foldLeft(Evented(world)) { case (w, spawner) =>
        w.events ++: act(w.value, spawner)(log.prefixed(s"GrowingSpawnerAI[$spawner]|"))
      }
  }

  def act(world: World, spawner: GrowingSpawner)(implicit log: LoggingAdapter): Result = {
    @tailrec def work(
      actionsLeft: SpawnerStr, world: Evented[World], readyUnits: List[spawner.Controlled]
    ): Result = actionsLeft match {
      case i if i <= SpawnerStr(0) => world
      case _ =>
        val newActions = actionsLeft - SpawnerStr(1)
        readyUnits match {
          case unit :: rest =>
            work(newActions, world.flatMap(act(_, unit)), rest)
          case Nil => spawn(world.value, spawner) match {
            case Left(err) =>
              log.error(err)
              world
            case Right(newWorld) =>
              work(
                newActions, world.events ++: newWorld.map(_._1),
                newWorld.value._2.filter(_.isWarpedIn).fold2(List.empty, List(_))
                  ::: readyUnits
              )
        }
      }
    }

    val readyUnits = world.objects.collect {
      case unit: spawner.Controlled
        if unit.owner === spawner.owner && unit.hasAttacksLeft && unit.isWarpedIn => unit
    }.toList

    work(spawner.strength, Evented(world), readyUnits)
  }

  def act(
    world: World, unit: GrowingSpawner#Controlled
  )(implicit log: LoggingAdapter): Result = {
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
          target <- possibleTargets.filter(_.companion.isCritical) match {
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
          _.map(_._1)
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
    world: World, spawner: GrowingSpawner
  ): Either[String, WObject.WorldObjOptUpdate[spawner.Controlled]] = {
    Random.shuffle(spawner.visibility.points).map(spawner.spawn(world, _)).
      find(_.isRight).getOrElse(
        s"$spawner couldn't find a place to spawn a unit within ${spawner.visibility}".
          left
      )
  }
}
