package app.models.game.world

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

  def isNextTo(v: Vect2) = {
    if (x == v.x && y == v.y) false
    else math.abs(x - v.x) <= 1 && math.abs(y - v.y) <= 1
  }

  def radialDistance(v: Vect2) = RadialDistance(distance(v).toFloat)
  def distance(v: Vect2) = math.sqrt(sqrDistance(v))
  def sqrDistance(v: Vect2) = math.pow(x - v.x, 2) + math.pow(y - v.y, 2)

  def tileDistance(v: Vect2): TileDistance =
    TileDistance(math.abs(x - v.x) + math.abs(y - v.y))
  def tileDistance(b: Bounds): TileDistance = b.perimeter.map(tileDistance).min

  def movementDistance(v: Vect2): Movement =
    Movement.fromAbsolute((distance(v) * Movement.Multiplier).toInt)

  def toBounds = Bounds(this, Vect2.one)
}
