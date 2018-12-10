import scala.io.StdIn.readLine

def readInput: Option[(Int, Int, Int)] = "(\\d+) players; last marble is worth (\\d+) points".r.findFirstMatchIn(readLine).map(m => (m.group(1).toInt, m.group(2).toInt, 0))

val (players, lastMarble, expected) = readInput.get
// val (players, lastMarble, expected) = (9, 25, 32)
// val (players, lastMarble, expected) = (10, 1618, 8317)

case class Turn(currentPlayer: Int, currentMarble: Int, marbles: List[Int], scores: List[Int]) {
  override def toString: String = s"[$currentPlayer] " + marbles.zipWithIndex.map { case (m, i) => if (i == currentMarble) { s"($m)" } else { s" $m " } }.mkString(" ")

  def nextPlayer: Int = (currentPlayer % scores.length) + 1
}

def playGame(turn: Turn, marbleValue: Int): Turn =
  marbleValue match {
    case m if m % 23 == 0 =>
      val removedMarbleIndex = (turn.currentMarble - 7 + turn.marbles.length) % turn.marbles.length
      val removedMarbleScore = turn.marbles(removedMarbleIndex)
      val newScore = List(m + removedMarbleScore + turn.scores(turn.currentPlayer - 1))
    val newMarbles = turn.marbles.zipWithIndex.filterNot { case (v, k) => k == removedMarbleIndex } .map(_._1)
    Turn(turn.nextPlayer, (removedMarbleIndex % turn.marbles.length), newMarbles, turn.scores.patch(turn.currentPlayer -1, newScore, 1))
    case 1 => Turn(turn.nextPlayer, 1, List(0, 1), turn.scores)
    case m =>
      val insertIndex = {
        (turn.currentMarble + 2) % turn.marbles.length match {
          case c if c == 0 && turn.marbles.length % 2 == 1 => turn.marbles.length
          case c => c
        }
      }
      val marbles = turn.marbles.patch(insertIndex, List(m), 0)
      Turn(turn.nextPlayer, insertIndex, marbles, turn.scores)
  }

val turn0 = Turn(0, 0, List(0), List.fill(players)(0))

// Print board on each turn :
// (1 to lastMarble).scanLeft(turn0)(playGame).foreach(println)

val lastTurn = (1 to lastMarble).foldLeft(turn0)(playGame)

// Players scores :
// lastTurn.scores.zipWithIndex.foreach { case (v, k) => println(s"Player ${k + 1}: $v") }

println("Computed best score: " + lastTurn.scores.max)

if (expected > 0) {
  println(s"Expected: $expected")
}

