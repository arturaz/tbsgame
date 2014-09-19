package app

import app.models.game.ai.{GrowingSpawnerAI, SingleMindAI}
import app.models.world.World
import app.models.world.buildings.GrowingSpawner
import app.models.{Player, Team}
import implicits._
import infrastructure.RenderPNG

import scala.util.Random

object TestMain {
  def main(args: Array[String]): Unit = {
    Random.setSeed(345345334)

    val playersTeam = Team(-1000)
    val waspPlayers = Vector.tabulate(4)(i => Player(i, s"wasp-ai-$i", Team(-i)))
    val spawnerTeams = Vector.tabulate(3)(Team)
    var spawnerPlayers = Vector.empty[Player]
    def spawnerPlayer = {
      val id = Random.nextInt(10000)
      val player = Player(id, s"spawner-ai-$id", spawnerTeams.random.get)
      spawnerPlayers :+= player
      player
    }

    lazy val ais =
      waspPlayers.map { player => (player, SingleMindAI.act(_: World, player)) } ++
      spawnerPlayers.map { player => (player, GrowingSpawnerAI.act(_: World, player)) }

    var worlds = Vector.empty[(World, String)]
    def renderWorlds(force: Boolean=false): Unit = if (force || worlds.size >= 50) {
      worlds.par.foreach { case (world, path) =>
        println(path)
        RenderPNG.world(world, path)
      }
      worlds = Vector.empty
    }

    def run(world: World, turn: Int=1): Unit = {
      println(s"Turn $turn starting.")
//      worlds :+= (world, f"world-$turn%03d")
      val newWorld = ais.zipWithIndex.foldLeft(world) { case (pWorld, ((aiPlayer, f), idx)) =>
        println(s"$aiPlayer ($idx) is going.")
        val (newWorld, events) = f(pWorld)
        events.foreach(println)
        if (events.nonEmpty)
          worlds :+= (pWorld, f"world-$turn%03d-$idx%03d")
        newWorld
      }.nextTurn
      renderWorlds()

      if (newWorld.teams.size > 1) run(newWorld, turn + 1)
      else worlds :+= (newWorld, f"world-$turn%03d-final")
    }

    run(World.create(
      playersTeam, () => waspPlayers.random.get, () => spawnerPlayer,
      spawners = 7
    ).updateAll { case sp: GrowingSpawner => sp.withTurnsPerStrength({
      (1 to 20).random
    }) })
    renderWorlds(force = true)
  }
}
