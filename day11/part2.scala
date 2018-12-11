import scala.io.StdIn.readInt

val MAX_X = 300
val MAX_Y = 300

case class Pos(x: Int, y: Int)

def cellPower(serial: Int)(p: Pos): Int = {
  val rackId = p.x + 10
  (((((rackId * p.y) + serial) * rackId) / 100) % 10) - 5
}

// TODO Should not recompute stuff each time
def squarePower(serial: Int, sqSize: Int)(p: Pos): Int = {
  (p.x to (p.x + sqSize - 1)).flatMap(x => (p.y to (p.y + sqSize - 1)).map(y => Pos(x, y))).map(cellPower(serial)).sum
}

/*
println(cellPower(57)(Pos(122, 79)) + " expected -5")
println(cellPower(39)(Pos(217, 196)) + " expected 0")
println(cellPower(71)(Pos(101, 153)) + " expected 4")
println(squarePower(18)(Pos(33,45)) + " expected 29")
// */

val input = readInt

// Suboptimal, but works.
Iterator.from(3, 1)
  .scanLeft((0, 0, None: Option[Pos])) {
    case ((max, maxSize, maxP), sqSize) => val p = (1 to (MAX_X - sqSize)).flatMap(x => (1 to (MAX_Y - sqSize)).map(y => Pos(x, y))).maxBy(squarePower(input, sqSize)) 
    val pPower = squarePower(input, sqSize)(p)
    if (pPower > max) {
      (pPower, sqSize, Some(p))
    } else {
      null
    }
    case _ => null
  }
  .takeWhile(_ != null)
  .map { case (max, size, pos) => s"Size $size -> Best pos = $pos with power $max" }
  .foreach(println)
