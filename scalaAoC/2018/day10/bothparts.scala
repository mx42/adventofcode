import scala.io.StdIn.readLine
import scala.annotation.tailrec

val MAX_X = 70
val MAX_Y = 30

case class Point(x: Int, y: Int, xv: Int, yv: Int) {
  lazy val next: Point = Point(x + xv, y + yv, xv, yv)
}

object Point {
  def apply(in: String): Point = 
    "position=<((?:-|\\s)?\\d+), ((?:-|\\s)?\\d+)> velocity=<((?:-|\\s)?\\d+), ((?:-|\\s)?\\d+)".r.findFirstMatchIn(in).map {
      m => Point(m.group(1).trim.toInt, m.group(2).trim.toInt, m.group(3).trim.toInt, m.group(4).trim.toInt)
   }.get
}

def maxDistances(pts: List[Point]): (Int, Int) =
  (pts.map(_.x).max - pts.map(_.x).min,
   pts.map(_.y).max - pts.map(_.y).min)

def movePoints(pts: List[Point]): List[Point] = pts.map(_.next)

val points = Iterator
  .continually(readLine)
  .takeWhile(_ != null)
  .map(Point.apply)
  .toList

@tailrec
def iterateWhileUndrawable(pts: List[Point], it: Int): (List[Point], Int) = maxDistances(pts) match {
  case (x, y) if x < MAX_X && y < MAX_Y => (pts, it)
  case _ => iterateWhileUndrawable(movePoints(pts), it + 1)
}

def printPoints(pts: List[Point], size: ((Int, Int),(Int, Int))): Unit = 
  (size._1._1 to size._1._2).foreach { y =>
    (size._2._1 to size._2._2).foreach { x =>
      if (pts.exists(p => p.x == x && p.y == y)) {
        print("#")
      } else {
        print(".")
      }
    }
    println("")
 }

val (drawablePoints, turns) = iterateWhileUndrawable(points, 0)

val drawZone = (
  (drawablePoints.map(_.y).min, drawablePoints.map(_.y).max),
  (drawablePoints.map(_.x).min, drawablePoints.map(_.x).max))

println(s"Visibility at ${turns}s")
printPoints(drawablePoints, drawZone)
