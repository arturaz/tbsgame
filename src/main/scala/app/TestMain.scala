package app

import app.models.game.ai.SingleMindAI
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

    var events = Vector.empty[Event]
    Stream.from(1).takeWhile(i => i == 1 || events.isEmpty).foldLeft(World.create(
      playersTeam, () => waspPlayers.random.get, () => {
        val id = Random.nextInt()
        Player(id, s"ai-$id", Team(-id))
      }
    )) { case (world, turn) =>
      println(s"Turn $turn starting.")
      RenderPNG.world(world, f"world-$turn%03d")
      waspPlayers.zipWithIndex.foldLeft(world) { case (pWorld, (aiPlayer, idx)) =>
        println(s"$aiPlayer ($idx) is going.")
        val (newWorld, newEvents) = SingleMindAI.act(pWorld, aiPlayer)
        events = newEvents
        events.foreach(println)
        RenderPNG.world(pWorld, f"world-$turn%03d-$idx%03d")
        newWorld
      }.nextTurn
    }
  }
}
