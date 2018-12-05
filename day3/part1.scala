import scala.io.StdIn.readLine

case class Claim(id: Int, x: Int, y: Int, w: Int, h: Int) {
  val xe: Int = x + (w - 1)
  val ye: Int = y + (h - 1)

  def getSquares: Seq[(Int, Int)] =
    for {
      xi <- x to xe
      yi <- y to ye
    } yield (xi, yi)
}

object Claim {
  def apply(input: String): Claim =
    "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)".r.findFirstMatchIn(input).map {
      m => Claim(m.group(1).toInt, m.group(2).toInt, m.group(3).toInt, m.group(4).toInt, m.group(5).toInt)
    }.get
}

val input = Iterator
  .continually(readLine)
  .takeWhile(_ != null)
  .map(Claim.apply)

val res = input
  .flatMap(_.getSquares)
  .foldLeft(Map.empty[(Int, Int), Int]) {
    (acc, sq) => acc + (sq -> (acc.getOrElse(sq, 0) + 1))
  }
  .count(_._2 > 1)

println(res)
