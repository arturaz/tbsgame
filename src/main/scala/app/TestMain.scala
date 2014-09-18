package app

import app.models.game.ai.SingleMindAI
import app.models.world.World
import app.models.{Player, Team}
import implicits._
import infrastructure.RenderPNG

import scala.util.Random

object TestMain {
  def main(args: Array[String]): Unit = {
    Random.setSeed(10)

    val playersTeam = Team(-1000)
    val waspPlayers = Vector.tabulate(4)(i => Player(i, s"wasp-ai-$i", Team(-i)))
    var oldWorld = World.create(
      playersTeam, () => waspPlayers.random.get, () => {
        val id = Random.nextInt()
        Player(id, s"ai-$id", Team(-id))
      }
    )

    var world = oldWorld
    Stream.from(1).takeWhile(i => i == 1 || world != oldWorld).foreach { turn =>
      println(s"Turn $turn starting.")
      RenderPNG.world(world, f"world-$turn%03d")
      waspPlayers.zipWithIndex.foreach { case (aiPlayer, idx) =>
        println(s"$aiPlayer ($idx) is going.")
        val (newWorld, events) = SingleMindAI.act(world, aiPlayer)
        events.foreach(println)
        oldWorld = world
        world = newWorld
        RenderPNG.world(world, f"world-$turn%03d-$idx%03d")
      }
      world = world.nextTurn
    }
  }
}
