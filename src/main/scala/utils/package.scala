import scalaz.\/

/**
 * Created by arturas on 2014-11-17.
 */
package object utils {
  type ErrOpt[A] = String \/ A
}
