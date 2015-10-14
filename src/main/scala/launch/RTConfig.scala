package launch

import app.actors.NetClient
import com.typesafe.config.Config
import spire.math.UInt

import scalaz._, Scalaz._

/** Runtime config that is being loaded from Typesafe config. **/
case class RTConfig(
  port: UInt, dbUrl: String, controlKey: NetClient.Control.SecretKey
)

object RTConfig {
  def key(s: String) = s"tbsgame.$s"

  def fromConfig(config: Config): ValidationNel[String, RTConfig] = {
    val port = config.readUInt(key("server.port"))
    val dbUrl = config.readStr(key("db.url"))
    val configSecretKey =
      config.readStr(key("control.secret_key")).map(NetClient.Control.SecretKey.apply)

    (
      port.validation.toValidationNel |@|
      dbUrl.validation.toValidationNel |@|
      configSecretKey.validation.toValidationNel
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
  }
}
