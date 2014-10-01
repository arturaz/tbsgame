package app.models.game.ai

import implicits._

import app.algorithms.Pathfinding.SearchRes
import app.algorithms.{Combat, Pathfinding}
import app.models.Owner
import app.models.game.events.Evented
import app.models.world.buildings.GrowingSpawner
import app.models.world.{OwnedObj, WObject, World}
import infrastructure.Log

import scala.annotation.tailrec
import scala.util.Random

/**
 * Created by arturas on 2014-09-18.
 */
object GrowingSpawnerAI {
  type Result = Evented[World]

  def act(world: World, owner: Owner): Result = {
    val spawners = world.objects.collect {
      case spawner: GrowingSpawner if spawner.owner == owner => spawner
    }

    if (spawners.isEmpty)
      SingleMindAI.act(world, owner)
    else
      spawners.foldLeft(Evented(world)) { case (w, spawner) =>
        w.events ++: act(w.value, spawner)
      }
  }

  def act(world: World, spawner: GrowingSpawner): Result = {
    @tailrec def work(
      actionsLeft: Int, world: Evented[World], readyUnits: List[spawner.Controlled]
    ): Result = actionsLeft match {
      case i if i <= 0 => world
      case _ =>
        val newActions = actionsLeft - 1
        readyUnits match {
          case unit :: rest =>
            work(newActions, world.flatMap(act(_, unit)), rest)
          case Nil => spawn(world.value, spawner) match {
            case Left(err) =>
              Log.error(err)
              world
            case Right(newWorld) =>
              work(
                newActions, world.events ++: newWorld.map(_._1),
                newWorld.value._2.fold2(List.empty, List(_)) ::: readyUnits
              )
        }
      }
    }

    val readyUnits = world.objects.collect {
      case unit: spawner.Controlled
        if unit.owner == spawner.owner && ! unit.hasAttacked => unit
    }.toList

    work(spawner.strength, Evented(world), readyUnits)
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
    opt.fold(Evented(world))(Combat.moveAttackLoose(world, unit, _))
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
