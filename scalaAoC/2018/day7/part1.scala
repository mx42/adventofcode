import scala.io.StdIn.readLine

// "Step A must be finished before step B can begin."
//  0    1 2    3  4        5      6    7 8   9
val PREREQ_STEP_INDEX: Int = 1
val STEP_INDEX: Int = 7

val stepsWithParents = Iterator.continually(readLine)
  .takeWhile(_ != null)
  .map { l =>
    val ws = l.split(" ")
    ws(PREREQ_STEP_INDEX).head -> ws(STEP_INDEX).head }
  .foldLeft(Map.empty[Char, Set[Char]]){
    case (acc, (parent, child)) => acc +
      (child -> (acc.getOrElse(child, Set.empty[Char]) + parent)) +
      (parent -> (acc.getOrElse(parent, Set.empty[Char])))}
  .toList

def walkGraph(g: List[(Char, Set[Char])], result: String = ""): String = g match {
  case Nil => result
  case _ => 
    val chosenParent = g.filter(_._2.isEmpty).map(_._1).min
    walkGraph(
      g
        .filter(_._1 != chosenParent)
        .map { case (k, v) => k -> v.filter(_ != chosenParent) },
      result + chosenParent.toString)
}

println(walkGraph(stepsWithParents))
