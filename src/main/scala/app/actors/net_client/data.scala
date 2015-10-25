package app.actors.net_client

import java.util.UUID
import implicits._

sealed trait AuthToken

case class SessionToken(value: String) extends AuthToken
object SessionToken {
  def random() = SessionToken(UUID.randomUUID().shortStr)
}

case class PlainPassword(value: String) extends AuthToken {
  import com.github.t3hnar.bcrypt._

  def encrypted = value.bcrypt
  def check(hash: String) = value.isBcrypted(hash)
}

case class Credentials(name: String, auth: AuthToken) {
  def check(sessionToken: String, passwordHash: String): Boolean =
    auth match {
      case SessionToken(token) => sessionToken == token
      case password: PlainPassword => password.check(passwordHash)
    }
}

case class ControlSecretKey(key: String) extends AnyVal