package utils

import java.util.UUID
import implicits._

/**
 * Created by arturas on 2014-10-26.
 */
trait IdObj extends Any {
  def id: UUID
  protected def prefix: String
  override def toString = s"$prefix[${id.shortStr}}]"
}
