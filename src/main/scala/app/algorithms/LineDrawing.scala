package app.algorithms

import app.models.game.world.Vect2
import utils.data.NonEmptyVector

/* http://tech-algorithm.com/articles/drawing-line-using-bresenham-algorithm/ */
object LineDrawing {
  def line(start: Vect2, end: Vect2): NonEmptyVector[Vect2] = {
    val w = end.x - start.x
    val h = end.y - start.y

    var dx1, dx2 = if (w < 0) -1 else if (w > 0) 1 else 0
    var dy1 = if (h < 0) -1 else if (h > 0) 1 else 0
    var dy2 = 0
    var longest = w.abs
    var shortest = h.abs
    if (! (longest > shortest)) {
      longest = h.abs
      shortest = w.abs
      if (h < 0) dy2 = -1
      else if (h > 0) dy2 = 1
      dx2 = 0
    }
    var numerator = longest >> 1

    var points = Vector.empty[Vect2]
    var idx = 0
    var x = start.x
    var y = start.y
    while (idx <= longest) {
      points :+= Vect2(x, y)
      numerator += shortest
      if (! (numerator < longest)) {
        numerator -= longest
        x += dx1
        y += dy1
      }
      else {
        x += dx2
        y += dy2
      }
      idx += 1
    }

    NonEmptyVector(points)
  }
}