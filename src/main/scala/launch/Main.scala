package launch

import java.nio.ByteOrder

import akka.actor.{ActorSystem, Props}
import app.actors.Server
import app.actors.game.GamesManagerActor
import app.models.game.world.maps.{TMXReader, GameMap}
import app.persistence.DBDriver
import org.apache.commons.io.{Charsets, IOUtils}
import org.flywaydb.core.Flyway
import utils.data.NonEmptyVector
import collection.JavaConverters._
import implicits._

/**
 * Created by arturas on 2014-10-08.
 */
object Main {
  private[this] lazy val appSystem = ActorSystem("app")

  def main(args: Array[String]) = {
    implicit val byteOrder = ByteOrder.BIG_ENDIAN

    val maps = NonEmptyVector.create(readMaps("maps")).getOrElse({
      System.err.println("No maps loaded. Exiting.")
      sys.exit(-1)
    })

    val dbUrl = "jdbc:sqlite:app.db"
    applyMigrations(dbUrl)
    val db = DBDriver.Database.forURL(dbUrl)

    val gamesManager = appSystem.actorOf(Props(new GamesManagerActor(maps)), "games-manager")
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

  def readMaps(path: String): Vector[GameMap] = {
    println("Loading maps...")
    val dirPath = if (path.endsWith("/")) path else s"$path/"
    val cl = getClass.getClassLoader
    val names = IOUtils.readLines(
      cl.getResourceAsStream(dirPath), Charsets.UTF_8
    ).asScala.filter(_.endsWith(".tmx")).toVector
    println(s"Maps discovered (${names.size}): ${names.mkString(",")}")
    names.flatMap(name =>
      TMXReader.read(cl.getResourceAsStream(s"$dirPath$name")).fold(
        err => {
          println(s"! $name: error while reading map - $err")
          None
        },
        map => {
          println(s"* $name loaded.")
          Some(map)
        }
      )
    )
  }
}
