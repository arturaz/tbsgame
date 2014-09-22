package app.models.game.ai

import app.algorithms.Pathfinding.SearchRes
import app.algorithms.{Combat, Pathfinding}
import app.models.Owner
import app.models.game.events.{Event, WarpEvt}
import app.models.world.buildings.GrowingSpawner
import app.models.world.{OwnedObj, World}
import infrastructure.Log

import scala.annotation.tailrec
import scala.util.Random

/**
 * Created by arturas on 2014-09-18.
 */
object GrowingSpawnerAI {
  type Result = (World, Vector[Event])

  def act(world: World, owner: Owner): Result = {
    val spawners = world.objects.collect {
      case spawner: GrowingSpawner if spawner.owner == owner => spawner
    }

    if (spawners.isEmpty)
      SingleMindAI.act(world, owner)
    else
      spawners.foldLeft(
        (world, Vector.empty[Event])
      ) { case ((world, events), spawner) =>
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
      case unit: spawner.Controlled
        if unit.owner == spawner.owner && ! unit.hasAttacked => unit
    }.toList

    work(spawner.strength, world, readyUnits, Vector.empty)
  }

  def act(
    world: World, unit: GrowingSpawner#Controlled
  ): Result = {
    val opt = for {
      target <- world.objects.collect {
        case fo: OwnedObj if fo.isEnemy(unit) => fo
      } match {
        case s if s.isEmpty => None
        case s => Some(s.minBy(_.bounds.tileDistance(unit.position)))
      }
      path <- Pathfinding.aStar(
        unit, target.bounds, world.bounds,
        unit.obstacles(world.objects).map(_.bounds)
      )
    } yield SearchRes(target, path)
    opt.fold((world, Vector.empty[Event]))(Combat.moveAttackLoose(world, unit, _))
  }

  private[this] def spawn(
    world: World, spawner: GrowingSpawner
  ): Either[String, (World, spawner.Controlled, WarpEvt)] = {
    Random.shuffle(spawner.visibility.points).foreach { point =>
      if (! world.objects.exists(_.bounds.contains(point))) {
        val unit = spawner.spawn(point)
        return Right((world.add(unit), unit, WarpEvt(unit)))
      }
    }

    Left(
      s"$spawner couldn't find a place to spawn a unit within ${
      spawner.visibility}"
    )
  }
}
