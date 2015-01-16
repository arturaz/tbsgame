package app.persistence

import java.util.UUID

import implicits._

object DBDriver extends scala.slick.driver.SQLiteDriver.SimpleQL
