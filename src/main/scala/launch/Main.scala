package launch

import java.nio.ByteOrder

import akka.actor.{ActorSystem, Props}
import app.actors.Server
import app.actors.game.GamesManagerActor
import app.persistence.DBDriver
import org.flywaydb.core.Flyway

/**
 * Created by arturas on 2014-10-08.
 */
object Main {
  private[this] lazy val appSystem = ActorSystem("app")

  def main(args: Array[String]) = {
    implicit val byteOrder = ByteOrder.BIG_ENDIAN

    val dbUrl = "jdbc:sqlite:app.db"
    applyMigrations(dbUrl)
    val db = DBDriver.Database.forURL(dbUrl)

    val gamesManager = appSystem.actorOf(Props(new GamesManagerActor), "games-manager")
    println(s"Games manager started: $gamesManager")
    val server = appSystem.actorOf(Props(new Server(5000, gamesManager, db)), "server")
    println(s"Server started: $server")
  }

  def applyMigrations(url: String): Unit = {
    val flyway = new Flyway()
    flyway.setDataSource(url, "", "")
    print("Applying pending migrations (if any)... ")
    val migrated = flyway.migrate()
    println(s"$migrated migrations applied.")
  }
}
