package app

import app.models.game.ai.AIController
import app.models.world.World
import app.models.{Player, Team}
import implicits._
import infrastructure.RenderPNG

import scala.util.Random

object TestMain {
  def main(args: Array[String]): Unit = {
    val playersTeam = Team(-1000)
    val waspPlayers = Vector.tabulate(4)(i => Player(i, s"wasp-ai-$i", Team(-i)))
    var oldWorld = World.create(
      playersTeam, () => waspPlayers.random.get, () => {
        val id = Random.nextInt()
        Player(id, s"ai-$id", Team(-id))
      }
    )
    RenderPNG.world(oldWorld, "world-start")

    var world: World = null
    Stream.from(1).takeWhile(_ => world != oldWorld).foreach { turn =>
      println(s"Turn $turn starting.")
      waspPlayers.zipWithIndex.foreach { case (aiPlayer, idx) =>
        println(s"$aiPlayer ($idx) is going.")
        val (newWorld, events) = AIController.act(world, aiPlayer)
        events.foreach(println)
        oldWorld = world
        world = newWorld
        RenderPNG.world(world, f"world-$turn%03d-$idx%03d")
      }
    }
  }
}
