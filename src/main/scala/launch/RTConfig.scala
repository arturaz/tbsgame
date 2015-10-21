package launch

import java.util.concurrent.TimeUnit

import app.actors.NetClient
import com.typesafe.config.Config
import spire.math.UInt

import scala.concurrent.duration._
import scalaz._, Scalaz._

/**
 * Runtime config that is being loaded from Typesafe config.
 *
 * @param gcm if not set, GCM messages are not sent
 */
case class RTConfig(
  port: UInt, dbUrl: String, controlKey: NetClient.Control.SecretKey,
  gcm: Option[RTConfig.GCM], gamesManager: RTConfig.GamesManager
)

object RTConfig {
  // Time to live
  case class TTL(duration: FiniteDuration) extends AnyVal

  case class GCM(key: GCM.Key, searchForOpponentTTL: TTL)
  object GCM {
    case class Key(value: String) extends AnyVal
  }

  case class GamesManager(backgroundHeartbeatTTL: TTL)

  def key(s: String) = s"tbsgame.$s"

  def fromConfig(config: Config): ValidationNel[String, RTConfig] = {
    val port = config.readUInt(key("server.port"))
    val dbUrl = config.readStr(key("db.url"))
    val configSecretKey =
      config.readStr(key("control.secret_key")).map(NetClient.Control.SecretKey.apply)
    val gcmAuth = config.readBool(key("google.gcm.enabled")).flatMap {
      case false => None.right
      case true =>
        for {
          gcmKey <- config.readStr(key("google.gcm.server_api_key")).map(key => GCM.Key(key))
          searchForOpponentTTL <-
            config.readDuration(key("google.gcm.searching_for_opponent.time_to_live")).map(TTL)
        } yield Some(GCM(gcmKey, searchForOpponentTTL))
    }
    val gamesManager = config.readDuration(key("games_manager.background_heartbeat_ttl"))
      .map(d => GamesManager(TTL(d)))

    (
      port.validation.toValidationNel |@|
      dbUrl.validation.toValidationNel |@|
      configSecretKey.validation.toValidationNel |@|
      gcmAuth.validation.toValidationNel |@|
      gamesManager.validation.toValidationNel
    )(RTConfig.apply)
  }

  implicit class ConfigExts(val c: Config) extends AnyVal {
    type ConfigFn[A] = Config => String => A

    def read[A](key: String)(f: ConfigFn[A]): String \/ A = {
      try f(c)(key).right
      catch {
        case e: Exception => s"Error while reading '$key' from config: $e".left
      }
    }

    def readUInt(key: String) =
      read(key)(_.getInt).flatMap {
        case i if i < 0 => s"Key '$key' was negative ($i) when UInt was expected!".left
        case i => UInt(i).right
      }

    def readStr(key: String) = read(key)(_.getString)
    def readBool(key: String) = read(key)(_.getBoolean)
    def readDuration(key: String) =
      read(key)(c => key => c.getDuration(key, TimeUnit.MILLISECONDS).millis)
  }
}
