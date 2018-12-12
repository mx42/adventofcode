import scala.io.StdIn.readLine

val LAST_GEN = 130

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

val min = gens.map(_._2.min).min - 2
val max = gens.map(_._2.max).max + 2

gens.foreach(printGeneration(min, max))
