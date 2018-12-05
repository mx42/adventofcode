import scala.io.StdIn.readLine

case class Claim(id: Int, x: Int, y: Int, w: Int, h: Int) {
  val xe: Int = x + (w - 1)
  val ye: Int = y + (h - 1)

  def getSquares: Seq[((Int, Int), Int)] =
    for {
      xi <- x to xe
      yi <- y to ye
    } yield ((xi, yi) -> id)
}

object Claim {
  def apply(input: String): Claim =
    "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)".r.findFirstMatchIn(input).map {
      m => Claim(m.group(1).toInt, m.group(2).toInt, m.group(3).toInt, m.group(4).toInt, m.group(5).toInt)
    }.get
}

val claims = Iterator
  .continually(readLine)
  .takeWhile(_ != null)
  .map(Claim.apply)
  .toList

val fabric = claims
  .flatMap(_.getSquares)
  .foldLeft(Map.empty[(Int, Int), Set[Int]]) {
    (acc, sq) => acc + (sq._1 -> (acc.getOrElse(sq._1, Set.empty[Int]) + sq._2))
  }

val overlapping = fabric.filter(_._2.size > 1).values.flatten.toSet

println(claims.filterNot(c => overlapping.contains(c.id)))
