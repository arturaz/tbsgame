package launch

import java.nio.ByteOrder

import akka.actor.{ActorSystem, Props}
import app.actors.{NetClient, Server}
import app.actors.game.GamesManagerActor
import app.models.game.world.maps.{GameMaps, TMXReader, GameMap}
import app.persistence.DBDriver
import com.typesafe.config.{ConfigFactory, Config}
import org.apache.commons.io.{Charsets, IOUtils}
import org.flywaydb.core.Flyway
import utils.JAR
import utils.data.NonEmptyVector
import collection.JavaConverters._
import implicits._

import scalaz._, Scalaz._
import scalaz.effect.IO

/**
 * Created by arturas on 2014-10-08.
 */
object Main {
  private[this] lazy val appSystem = ActorSystem("app")
  private[this] implicit val byteOrder = ByteOrder.BIG_ENDIAN

  def main(args: Array[String]) = {
    implicit val rtConfig = RTConfig.fromConfig(ConfigFactory.load()).valueOr { errors =>
      System.err.println(s"Errors while loading runtime config: $errors. Exiting.")
      sys.exit(-1)
    }

    val io =
      if (args.isEmpty) runServer()
      else runControlClient(ImmutableArray.fromArray(args))
    io.unsafePerformIO()
  }

  def runServer()(implicit rtConfig: RTConfig): IO[Unit] = IO {
    val maps = readMaps("maps").valueOr { errors =>
      System.err.println(s"Errors in map loading: $errors. Exiting.")
      sys.exit(-1)
    }

    applyMigrations(rtConfig.dbUrl)
    val db = DBDriver.Database.forURL(rtConfig.dbUrl)

    val gamesManager = appSystem.actorOf(Props(new GamesManagerActor(maps)), "games-manager")
    println(s"Games manager started: $gamesManager")
    val server = appSystem.actorOf(Props(new Server(rtConfig, gamesManager, db)), "server")
    println(s"Server started: $server")
  }

  def runControlClient(args: ImmutableArray[String])(implicit rtConfig: RTConfig): IO[Unit] = {
    for {
      response <- args.head match {
        case "shutdown" => ControlClient.sendShutdown
        case "status" => ControlClient.sendStatusReq
        case other => IO { s"Unknown command: '$other'".left }
      }
      _ <- IO.putStrLn(response.fold(
        err => s"ERROR: $err",
        res => res.toString
      ))
    } yield ()
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

    (read("pve") |@| read("pvp")) { (pve, pvp) =>
      val pvpByPlayers =
        pvp.v.groupBy(_.startingPositions.size).mapValues(NonEmptyVector.apply)
      GameMaps(pve, pvpByPlayers)
    }
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
