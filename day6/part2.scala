import scala.io.StdIn.readLine

case class Pos(x: Int, y: Int) {
  def dist(other: Pos): Int = (other.x - x).abs + (other.y - y).abs
}

case class Coords(id: Int, p: Pos)

case class CoordsWithDist(c: Coords, dist: Int)

val MAX_X: Int = 400
val MAX_Y: Int = 400

val MAX_DIST_SUM: Int = 10000

val coords: List[Coords] = Iterator
  .continually(readLine)
  .takeWhile(_ != null)
  .map { s =>
    val p = s.split(", ")
    Pos(p(0).toInt, p(1).toInt)
  }
  .toList
  .zipWithIndex
  .map(x => Coords(x._2, x._1))

val distSum = for {
  x <- 0 to MAX_X
  y <- 0 to MAX_Y
  p = Pos(x, y)
  c = coords.map(_.p.dist(p)).sum if c < MAX_DIST_SUM
} yield (p, c)

println(distSum.size)
