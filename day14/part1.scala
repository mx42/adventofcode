import scala.io.StdIn.readInt

case class Turn(recipes: Vector[Int], idx1: Int, idx2: Int, turnNb: Int) {
  def newScores(a: Int, b: Int): Vector[Int] =
    (a + b).toString.map(_.toInt - '0').toVector

  def next: Turn = {
    val newRecipes = recipes ++ newScores(recipes(idx1), recipes(idx2))
    val newIdx1 = (idx1 + (newRecipes(idx1) + 1)) % newRecipes.size
    val newIdx2 = (idx2 + (newRecipes(idx2) + 1)) % newRecipes.size
    Turn(newRecipes, newIdx1, newIdx2, turnNb + 1)
  }

  override def toString: String =
    recipes.zipWithIndex.map {
      case (v, k) => k match {
        case _ if k == idx1 => s"($v)"
        case _ if k == idx2 => s"[$v]"
        case _ => s" $v "
      }
    }.mkString(" ")

  def computeScoreAt(n: Int): Option[String] =
    recipes match {
      case _ if recipes.size < n + 10 => None
      case _ => Some(recipes.slice(n, n + 10).mkString(""))
    }
}

val input = readInt

val turn0 = Turn(Vector(3, 7), 0, 1, 0)

val endTurn = Iterator.iterate(turn0)(_.next)
  .filter(_.recipes.length >= input + 11)
  .next

// println(s"Score: ${endTurn.computeScoreAt(5)} - Expected: 0124515891")
// println(s"Score: ${endTurn.computeScoreAt(18)} - Expected: 9251071085")
// println(s"Score: ${endTurn.computeScoreAt(2018)} - Expected: 5941429882")

println(s"Score: ${endTurn.computeScoreAt(input).get}")
