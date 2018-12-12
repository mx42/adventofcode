import scala.io.StdIn.readLine

val LAST_GEN = 5000

case class Rule(m2: Boolean, m1: Boolean, o: Boolean, p1: Boolean, p2: Boolean, r: Boolean) {
  def computePot(n: Int, input: Set[Int]): Option[Int] =
      if (input.contains(n - 2) == m2 &&
          input.contains(n - 1) == m1 &&
          input.contains(n) == o &&
          input.contains(n + 1) == p1 &&
          input.contains(n + 2) == p2 &&
          r) {
            Some(n)
          } else {
            None
          }
}

def parseRule(in: String): Option[Rule] =
  (("(#|\\.)" * 5) + " => (#|\\.)").r.findFirstMatchIn(in).map(
    m => Rule(m.group(1) == "#", m.group(2) == "#", m.group(3) == "#", m.group(4) == "#", m.group(5) == "#", m.group(6) == "#")
  )

def computeGeneration(current: Set[Int], rules: List[Rule]): Set[Int] =
  ((current.min - 2).to(current.max + 2))
    .flatMap(k => rules.flatMap(_.computePot(k, current)).headOption)
    .toSet

def printGeneration(min: Int, max: Int)(gen: (Int, Set[Int])): Unit = gen match {
  case (id, pots) =>
    val chars = pots.map(k => k -> '#').toMap.withDefaultValue('.')
    val str = List.range(min, max, 1).map(chars).mkString("")
    val sum = pots.sum
    println(s"$id: $str $sum")
}

val initialStateReg = "^initial state: ((?:#|\\.)+)$".r

val origInit = readLine

val init = initialStateReg.findFirstMatchIn(origInit)
  .map(m => m.group(1)).get
  .zipWithIndex.flatMap {
    case ('.', i) => None
    case ('#', i) => Some(i)
  }
.toSet

val rules = Iterator
  .continually(readLine)
  .takeWhile(_ != null)
  .flatMap(parseRule)
  .toList

val gens = List.range(1, LAST_GEN + 1, 1).scanLeft(0 -> init) {
  case (gen, id) => id -> computeGeneration(gen._2, rules) }
// val min = gens.map(_._2.min).min - 2
// val max = gens.map(_._2.max).max + 2
// gens.foreach(printGeneration(min, max))
/**
 * After ~100 generations, the same pattern simply moves
 * 38 pots
 * min pot = generation id - 80
 * max pot = generation id + 98
 #....#....#....#....#....#....#....#....#....#....#....#....#....#....#....#....#....#...#....#...#...#...#....#....#....#....#....#....#....#...#...#....#...#....#....#....#....#
 *
 * Observed that :
 * GenerationId = (Sum - 4) / 38 - 10
 * -> Sum = ((GenerationId + 10) * 38) + 4
 */

def printGenData(id: Int, gen: Set[Int]): Unit = {
  val cnt = gen.size
  val sum = gen.sum
  val x = (sum - 4) / 38.0 - 10
  val guess = ((id + 10) * 38) + 4

  println(s"Generation $id")
  println(s"CNT(p) = $cnt")
  println(s"SUM(p) = $sum")
  println(x)
  println(s"Guess = $guess")
  println("")
}

List(100, 101, 110, 120, 130, 140, 150, 200, 300, 500, 1000, 5000).foreach(g => printGenData(g, gens(g)._2))

println((50000000000L + 10) * 38 + 4)
