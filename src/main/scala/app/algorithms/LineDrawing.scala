package app.algorithms

import app.models.game.world.Vect2
import utils.data.NonEmptyVector

object LineDrawing {
  def line(start: Vect2, end: Vect2): NonEmptyVector[Vect2] =
    supercoverLineCircled(start, end)

  /* http://www.redblobgames.com/grids/line-drawing.html */
  def supercoverLine(start: Vect2, end: Vect2): NonEmptyVector[Vect2] = {
    val dx = end.x - start.x
    val dy = end.y - start.y
    val nx = dx.abs
    val ny = dy.abs
    val signX = if (dx > 0) 1 else -1
    val signY = if (dy > 0) 1 else -1

    var points = Vector.empty[Vect2]
    var x = start.x
    var y = start.y
    points :+= Vect2(x, y)

    var ix = 0
    var iy = 0
    while(ix < nx || iy < ny) {
      val a = (1+2*ix) * ny
      val b = (1+2*iy) * nx
      if (a == b) {
        // next step is diagonal
        x += signX
        y += signY
        ix += 1
        iy += 1
      } else if (a < b) {
        // next step is horizontal
        x += signX
        ix += 1
      } else {
        // next step is vertical
        y += signY
        iy += 1
      }
      points :+= Vect2(x, y)
    }
    NonEmptyVector(points)
  }

  def supercoverLineCircled(
    start: Vect2, end: Vect2, radius: Float = 0.4f
  ): NonEmptyVector[Vect2] = {
    val dx = end.x - start.x
    val dy = end.y - start.y
    val nx = dx.abs
    val ny = dy.abs
    val signX = if (dx > 0) 1 else -1
    val signY = if (dy > 0) 1 else -1

    var points = Vector.empty[Vect2]
    var x = start.x
    var y = start.y
    points :+= Vect2(x, y)

    val linePointDistanceSqr = (p1:Vect2, p2:Vect2, p:Vect2) => {
      val delta = p2 - p1
      val nom = delta.y*p.x - delta.x*p.y + p2.x*p1.y - p2.y*p1.x
      nom * nom
    }
    val sqrDist = dx*dx + dy*dy
    val sqrRadius = radius * radius
    def add(v: Vect2) = {
      if (linePointDistanceSqr(start, end, v) < sqrDist * sqrRadius)
        points :+= v
    }

    var ix = 0
    var iy = 0
    while(ix < nx || iy < ny) {
      val a = (1+2*ix) * ny
      val b = (1+2*iy) * nx
      if (a == b) {
        // next step is diagonal
        x += signX
        y += signY
        ix += 1
        iy += 1
      } else if (a < b) {
        // next step is horizontal
        x += signX
        ix += 1
      } else {
        // next step is vertical
        y += signY
        iy += 1
      }
      add(Vect2(x, y))
    }
    NonEmptyVector(points)
  }

  /* http://tech-algorithm.com/articles/drawing-line-using-bresenham-algorithm/ */
  def bresenhamLine(start: Vect2, end: Vect2): NonEmptyVector[Vect2] = {
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
