package utils

case class ValWithMax[A](value: A, max: A)(implicit num: Numeric[A]) {
  import num._

  def withValue(f: A => A) = copy(value = f(value))
  def withMax(f: A => A) = copy(max = f(max))

  def left = max - value
}
