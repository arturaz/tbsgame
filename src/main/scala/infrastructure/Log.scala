package infrastructure

/**
 * Created by arturas on 2014-09-11.
 */
object Log {
  var DebugEnabled = true

  def debug(s: => String) { if (DebugEnabled) println(s"[DEBUG]> $s")}
  def error(s: String) = Console.err.println(s"[ERROR]> $s")
}
