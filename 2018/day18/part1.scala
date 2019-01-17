import scala.io.StdIn.readLine

val OPEN = '.'
val TREE = '|'
val LMBR = '#'

case class Pos(x: Int, y: Int) {
  lazy val adjacent = List(
    Pos(x - 1, y - 1),
    Pos(x - 1, y),
    Pos(x - 1, y + 1),
    Pos(x, y - 1),
    Pos(x, y + 1),
    Pos(x + 1, y - 1),
    Pos(x + 1, y),
    Pos(x + 1, y + 1)
  ).filter { case Pos(xx, yy) => xx >= 0 && xx < 50 && yy >= 0 && yy < 50 }

  def adjacentChars(forest: Map[Pos, Char]): List[Char] = adjacent.map(forest)
}

val forest = Iterator.continually(readLine)
  .takeWhile(_ != null)
  .map(_.zipWithIndex)
  .zipWithIndex
  .flatMap { case (line, y) =>
    line.map { case (char, x) => Pos(x, y) -> char }
  }.toMap

val forestEvolution = Iterator.from(1).scanLeft((0, forest)) {
  case ((_, forest), turn) => (turn, forest.map {
    case (p, OPEN) if p.adjacentChars(forest).count(_ == TREE) >= 3 => p -> TREE
    case (p, OPEN) => p -> OPEN
    case (p, TREE) if p.adjacentChars(forest).count(_ == LMBR) >= 3 => p -> LMBR
    case (p, TREE) => p -> TREE
    case (p, LMBR) if p.adjacentChars(forest).count(_ == LMBR) >= 1 && p.adjacentChars(forest).count(_ == TREE) >= 1 => p -> LMBR
    case (p, LMBR) => p -> OPEN
  })
}

def forestToString(forest: Map[Pos, Char]): String =
  (0 to 49).map(y => (0 to 49).map(x => forest(Pos(x, y))).mkString("")).mkString("\n")

val turns = forestEvolution
  .take(11)
  .toList
  .foreach { case (turn, map) =>
    println(s"Turn $turn")
    val charCounts = map.groupBy(_._2).mapValues(_.size)
    charCounts.foreach { case (c, cnt) => println(s"$c count: $cnt") }
    println(s"Score: ${charCounts(TREE) * charCounts(LMBR)}")
    println(forestToString(map))
  }
