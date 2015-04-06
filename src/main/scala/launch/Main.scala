package launch

import java.nio.ByteOrder

import akka.actor.{ActorSystem, Props}
import app.actors.Server
import app.actors.game.GamesManagerActor
import app.models.game.world.maps.{GameMaps, TMXReader, GameMap}
import app.persistence.DBDriver
import org.apache.commons.io.{Charsets, IOUtils}
import org.flywaydb.core.Flyway
import utils.JAR
import utils.data.NonEmptyVector
import collection.JavaConverters._
import implicits._

import scalaz._, Scalaz._

/**
 * Created by arturas on 2014-10-08.
 */
object Main {
  private[this] lazy val appSystem = ActorSystem("app")

  def main(args: Array[String]) = {
    implicit val byteOrder = ByteOrder.BIG_ENDIAN

    val maps = readMaps("maps").valueOr { errors =>
      System.err.println(s"Errors in map loading: $errors. Exiting.")
      sys.exit(-1)
    }

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

  def readMaps(path: String): ValidationNel[String, GameMaps] = {
    def read(name: String) =
      NonEmptyVector.create(readMapsFromDir(s"$path/$name"))
      .toSuccess(s"No $name maps found").toValidationNel

    (read("pve") |@| read("pvp"))(GameMaps.apply)
  }

  def readMapsFromDir(path: String): Vector[GameMap] = {
    val dirPath = if (path.endsWith("/")) path else s"$path/"
    println(s"Loading maps from $dirPath...")
    val cl = getClass.getClassLoader
    val names =
      JAR.getResourceListing(getClass, dirPath).filter(_.endsWith(".tmx")).toVector
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
