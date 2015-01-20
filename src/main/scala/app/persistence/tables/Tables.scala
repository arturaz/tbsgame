package app.persistence.tables

import scala.slick.lifted.TableQuery

/**
 * Created by arturas on 2015-01-20.
 */
object Tables {
  object users extends TableQuery(new Users(_))
}
