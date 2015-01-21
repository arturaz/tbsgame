package app.persistence.tables

import java.util.UUID

import app.models.User
import app.persistence.DBDriver

import scala.slick.lifted.Tag
import DBDriver._


class Users(tag: Tag) extends Table[
  (UUID, String, String, String, String, Boolean)
](tag, "users") {
  def id = column[UUID]("id", O.PrimaryKey, O.DBType("BLOB(16)"))
  def name = column[String]("name")
  def email = column[String]("email")
  def password = column[String]("password")
  def sessionToken = column[String]("session_token")
  def autogenerated = column[Boolean]("autogenerated")

  def idx1 = index("unique_name", name, unique=true)

  def * = (id, name, email, password, sessionToken, autogenerated)
  def user = (id, name) <> (User.tupled, User.unapply)
}