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
    Random.setSeed(3000)

    val playersTeam = Team(-1000)
    val waspPlayers = Vector.tabulate(4)(i => Player(i, s"wasp-ai-$i", Team(-i - 1)))
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

    def run(world: World, maxTurns: Int, turn: Int=1): Vector[World] = {
      var worlds = Vector.empty[World]
      println(s"Turn $turn starting.")
//      worlds :+= (world, f"world-$turn%03d")
      val newWorld = ais.zipWithIndex.foldLeft(world) { case (pWorld, ((aiPlayer, f), idx)) =>
//        println(s"$aiPlayer ($idx) is going.")
        val (newWorld, events) = f(pWorld)
//        events.foreach(println)
        if (events.nonEmpty) worlds :+= pWorld
        newWorld.teamTurnFinished(aiPlayer.team)
      }.gameTurnFinished

      if (turn < maxTurns && newWorld.teams.size > 1)
        worlds ++ run(newWorld, maxTurns, turn + 1)
      else
        worlds :+ newWorld
    }

    val worlds = run(World.create(
      playersTeam, () => waspPlayers.random.get, () => spawnerPlayer,
      spawners = 7
    ).updateAll { case sp: GrowingSpawner => sp.withTurnsPerStrength({
      (1 to 10).random
    }) }, 100)

    def renderWorlds(worlds: Vector[World]): Unit = {
      worlds.par.zipWithIndex.foreach { case (world, idx) =>
        print(".")
        RenderPNG.world(world, f"world-$idx%03d")
      }
    }

    print("Rendering")
    renderWorlds(worlds)
  }
}
