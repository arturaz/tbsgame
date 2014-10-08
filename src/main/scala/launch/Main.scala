package launch

import java.nio.ByteOrder

import akka.actor.{Props, ActorSystem}
import app.actors.Server
import app.actors.game.GamesManagerActor

/**
 * Created by arturas on 2014-10-08.
 */
object Main {
  private[this] lazy val appSystem = ActorSystem("app")

  def main(args: Array[String]) = {
    implicit val byteOrder = ByteOrder.BIG_ENDIAN

    val gamesManager = appSystem.actorOf(Props(new GamesManagerActor), "games-manager")
    println(s"Games manager started: $gamesManager")
    val server = appSystem.actorOf(Props(new Server(5000, gamesManager)), "server")
    println(s"Server started: $server")
  }
}
