package app.models.world

object Vect2 {
  def one = Vect2(1, 1)
}

case class Vect2(x: Int, y: Int) {
  def *(v: Int) = Vect2(x * v, y * v)
  def +(v: Vect2) = Vect2(x + v.x, y + v.y)
  def -(v: Vect2) = Vect2(x - v.x, y - v.y)
  def unary_- = Vect2(-x, -y)

  def up = Vect2(x, y + 1)
  def down = Vect2(x, y - 1)
  def left = Vect2(x - 1, y)
  def right = Vect2(x + 1, y)

  def distance(v: Vect2) = math.sqrt(sqrDistance(v))
  def sqrDistance(v: Vect2) = math.pow(x - v.x, 2) + math.pow(y - v.y, 2)

  def tileDistance(v: Vect2) = math.abs(x - v.x) + math.abs(y - v.y)
}
