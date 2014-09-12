import scala.util.Random

/**
 * Created by arturas on 2014-09-11.
 */
package object implicits {
  implicit class RangeExts(r: Range) {
    def random = r.start + Random.nextInt(r.end - r.start)
  }
}
