import scala.io.StdIn.readLine

case class Pos(x: Int, y: Int) {
  def dist(other: Pos): Int = (other.x - x).abs + (other.y - y).abs
}

case class Coords(id: Int, p: Pos)

case class CoordsWithDist(c: Coords, dist: Int)

val MAX_X: Int = 400
val MAX_Y: Int = 400

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

val closest: Seq[(Pos, Option[Coords])] = for {
  x <- 0 to MAX_X
  y <- 0 to MAX_Y
  p = Pos(x, y)
} yield {
  val closestWith = coords
    .map(c => CoordsWithDist(c, c.p.dist(p)))
    .sortBy(_.dist)
    .take(2)
  (closestWith(0), closestWith(1)) match {
    case (c1, c2) if c1.dist == c2.dist => (p, None)
    case (c1, _) => (p, Some(c1.c))
  }
}

closest
  .filter(_._2.nonEmpty)
  .groupBy(_._2)
  .filter {
    case (c, es) => !es.map(_._1).exists(p => List(0, MAX_X).contains(p.x) || List(0, MAX_Y).contains(p.y))
  }
  .mapValues(_.size)
  .toList
  .sortBy(_._2)
  .foreach(println)
