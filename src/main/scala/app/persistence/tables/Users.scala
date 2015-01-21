package app.persistence.tables

import java.util.UUID

import app.models.User
import app.persistence.DBDriver

import scala.slick.lifted.Tag
import DBDriver._


class Users(tag: Tag) extends Table[
  (UUID, String, Option[String], String, String)
](tag, "users") {
  def id = column[UUID]("id", O.PrimaryKey, O.DBType("BLOB(16)"))
  def name = column[String]("name")
  def email = column[Option[String]]("email")
  def password = column[String]("password")
  def sessionToken = column[String]("session_token")

  def idx1 = index("unique_name", name, unique=true)

  def * = (id, name, email, password, sessionToken)
  def user = (id, name) <> (User.tupled, User.unapply)
}