package app.models.game.ai

import app.algorithms.{Combat, Pathfinding}
import app.models.Player
import app.models.game.events.{Event, SpawnEvt}
import app.models.world.buildings.GrowingSpawner
import app.models.world.{FactionObj, World}
import infrastructure.Log

import scala.annotation.tailrec
import scala.util.Random

/**
 * Created by arturas on 2014-09-18.
 */
object GrowingSpawnerAI {
  type Result = (World, Vector[Event])

  def act(world: World, ai: Player): Result = {
    world.objects.collect {
      case spawner: GrowingSpawner if spawner.owner == ai => spawner
    }.foldLeft((world, Vector.empty[Event])) { case ((world, events), spawner) =>
      val (newWorld, newEvents) = act(world, spawner)
      (newWorld, events ++ newEvents)
    }
  }

  def act(world: World, spawner: GrowingSpawner): Result = {
    @tailrec def work(
      actionsLeft: Int, world: World, readyUnits: List[spawner.Controlled],
      events: Vector[Event]
    ): Result = actionsLeft match {
      case i if i <= 0 => (world, events)
      case _ =>
        val newActions = actionsLeft - 1
        readyUnits match {
        case unit :: rest =>
          val (nWorld, evts) = act(world, unit)
          work(newActions, nWorld, rest, events ++ evts)
        case Nil => spawn(world, spawner) match {
          case Left(err) =>
            Log.error(err)
            (world, events)
          case Right((nWorld, unit, event)) =>
            work(newActions, nWorld, unit :: readyUnits, events :+ event)
        }
      }
    }

    val readyUnits = world.objects.collect {
      case unit: spawner.Controlled if unit.owner == spawner.owner => unit
    }.toList

    work(spawner.strength, world, readyUnits, Vector.empty)
  }

  def act(world: World, unit: GrowingSpawner#Controlled): Result = {
    val enemies = world.objects.collect {
      case fo: FactionObj if fo.isEnemy(unit) => fo
    }
    Pathfinding.longRangeNearestAttackSearch(
      unit, enemies, unit.obstacles(world.objects).map(_.bounds)
    )(_.bounds).
      fold((world, Vector.empty[Event]))(Combat.moveAttackLoose(world, unit, _))
  }

  private[this] def spawn(
    world: World, spawner: GrowingSpawner
  ): Either[String, (World, spawner.Controlled, SpawnEvt)] = {
    Random.shuffle(spawner.visibility.points).foreach { point =>
      if (! world.objects.exists(_.bounds.contains(point))) {
        val unit = spawner.spawn(point)
        return Right((world.add(unit), unit, SpawnEvt(unit)))
      }
    }

    Left(
      s"$spawner couldn't find a place to spawn a unit within ${
      spawner.visibility}"
    )
  }
}
