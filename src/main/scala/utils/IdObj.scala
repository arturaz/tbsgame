package utils

import java.util.UUID

/**
 * Created by arturas on 2014-10-26.
 */
trait IdObj extends Any {
  def id: UUID
  protected def prefix: String
  override def toString = s"$prefix[${
    strPart(id.getLeastSignificantBits)}-${strPart(id.getMostSignificantBits)
  }}]"
  private[this] def strPart(l: Long) = Base36.encode(l)
}
