package launch

import java.nio.ByteOrder

import akka.actor.{Props => UTProps}
import akka.typed.ScalaDSL._
import akka.typed.{ActorSystem, Props}
import app.actors.GCMSender
import app.models.game.world.maps.{GameMap, GameMaps, TMXReader}
import app.persistence.DBDriver
import com.typesafe.config.ConfigFactory
import implicits._
import org.flywaydb.core.Flyway
import utils.JAR
import utils.data.NonEmptyVector

import scalaz.Scalaz._
import scalaz._
import scalaz.effect.IO

/**
 * Created by arturas on 2014-10-08.
 */
object Main {
  // This is used in clients as well! Think a lot before changing it.
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

    val main = ContextAware[Unit] { ctx =>
      val gcm = rtConfig.gcm.map { gcm =>
        val behaviour = GCMSender.behaviour(gcm.authHeader)
        val ref = ctx.spawn(Props(behaviour), "gcm-sender")
        println(s"GCM sender started: $ref")
        (ref.asUntyped, gcm)
      }
//
//      val gamesManager = ctx.actorOf(UTProps(new GamesManagerActor(maps, gcm)), "games-manager")
//      println(s"Games manager started: $gamesManager")
//      val server = ctx.actorOf(UTProps(new Server(rtConfig, gamesManager, db)), "server")
//      println(s"Server started: $server")

      Empty
    }
    ActorSystem("app", Props(main))
  }

  def runControlClient(args: ImmutableArray[String])(implicit rtConfig: RTConfig): IO[Unit] = {
    for {
      response <- args.head match {
        case "shutdown" => ControlClient.sendShutdown.map(_.map(r =>
          if (r.success) "Success"
          else s"Failure!${r.message.fold2("", m => s" Message: $m")}"
        ))
        case "status" => ControlClient.sendStatusReq.map(_.map(_.toString))
        case other => IO { s"Unknown command: '$other'".left }
      }
      _ <- IO.putStrLn(response.fold(err => s"ERROR: $err", identity))
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
