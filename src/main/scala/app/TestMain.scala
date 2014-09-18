package app

import app.models.game.ai.{GrowingSpawnerAI, SingleMindAI}
import app.models.world.World
import app.models.{Player, Team}
import implicits._
import infrastructure.RenderPNG

import scala.util.Random
import app.models.game.events.Event

object TestMain {
  def main(args: Array[String]): Unit = {
    Random.setSeed(10)

    val playersTeam = Team(-1000)
    val waspPlayers = Vector.tabulate(4)(i => Player(i, s"wasp-ai-$i", Team(-i)))
    var spawnerPlayers = Vector.empty[Player]
    def spawnerPlayer = {
      val id = Random.nextInt(10000)
      val player = Player(id, s"spawner-ai-$id", Team(-id))
      spawnerPlayers :+= player
      player
    }

    lazy val ais =
      waspPlayers.map { player => (player, SingleMindAI.act(_: World, player)) } ++
      spawnerPlayers.map { player => (player, GrowingSpawnerAI.act(_: World, player)) }

    def run(world: World, turn: Int=1): Unit = {
      println(s"Turn $turn starting.")
      RenderPNG.world(world, f"world-$turn%03d")
      val newWorld = ais.zipWithIndex.foldLeft(world) { case (pWorld, ((aiPlayer, f), idx)) =>
        println(s"$aiPlayer ($idx) is going.")
        val (newWorld, events) = f(pWorld)
        events.foreach(println)
        RenderPNG.world(pWorld, f"world-$turn%03d-$idx%03d")
        newWorld
      }.nextTurn
      if (newWorld.teams.size > 1) run(newWorld, turn + 1)
    }

    run(World.create(
      playersTeam, () => waspPlayers.random.get, () => spawnerPlayer
    ))
  }
}
