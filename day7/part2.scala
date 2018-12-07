import scala.io.StdIn.readLine

// "Step A must be finished before step B can begin."
//  0    1 2    3  4        5      6    7 8   9
val PREREQ_STEP_INDEX: Int = 1
val STEP_INDEX: Int = 7

val BASE_TIME_TO_COMPLETE: Int = 61 // 60 + 1 because index of letter starting at 0.
val WORKERS_NB: Int = 4

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

def timeToComplete(c: Char): Int = BASE_TIME_TO_COMPLETE + (c - 'A')

def updateResult(chosenParents: Seq[Char], children: Map[Char, Set[Char]], initial: Map[Char, Int]): Map[Char, Int] = {
  val updatedParents = initial.map {
    case (k, v) if chosenParents.contains(k) =>
      k -> (v + timeToComplete(k))
    case (k, v) => k -> v
  }

  updatedParents.map {
    case (k, v) if children.contains(k) && children(k).exists(chosenParents.contains) =>
      k -> (children(k).filter(chosenParents.contains).map(p => updatedParents(p)) + v).max
    case (k, v) => k -> v
  }
}

def walkGraph(g: List[(Char, Set[Char])], result: Map[Char, Int]): Map[Char, Int] =
  g match {
    case Nil => result
    case _ =>
      val chosenParents = g.filter(_._2.isEmpty).map(_._1).sorted.take(WORKERS_NB)
      walkGraph(
        g.filter(e => !chosenParents.contains(e._1))
          .map { case (k, v) => k -> v.filterNot(chosenParents.contains) },
        updateResult(chosenParents, g.toMap, result))
}

val timesToCompletion = walkGraph(stepsWithParents, stepsWithParents.map(_._1 -> 0).toMap)

println(timesToCompletion('C'))
