import java.time.LocalTime
import scala.io.StdIn.readLine
import scala.collection.mutable.ArrayBuffer

def readInput: Option[(Int, Int, Int)] = "(\\d+) players; last marble is worth (\\d+) points".r.findFirstMatchIn(readLine).map(m => (m.group(1).toInt, m.group(2).toInt, 0))

val (players, lastMarbleOriginal, expected) = readInput.get
// val (players, lastMarbleOriginal, expected) = (9, 25, 32)
// val (players, lastMarbleOriginal, expected) = (10, 1618, 8317)

val lastMarble = lastMarbleOriginal * 100

case class Turn(currentPlayer: Int, marbles: Vector[Int], scores: Array[Long]) {
  override def toString: String = s"[${currentPlayer + 1}] " + marbles.mkString(" ")

  lazy val nextPlayer: Int = (currentPlayer + 1) % scores.length

  def winTurn(marble: Int): Turn = {
    val t: Vector[Int] = marbles.take(8)
    val newMarbles: Vector[Int] = t.slice(6,7) ++ marbles.drop(8) ++ t.take(6)
    val addScore: Long = marble + t.last
    Turn(nextPlayer, newMarbles, updateScore(addScore))
  }

  def classicTurn(marble: Int): Turn =
    Turn(nextPlayer, addMarble(marble), scores)

  def updateScore(newScore: Long): Array[Long] = {
    val newScores: ArrayBuffer[Long] = ArrayBuffer.concat(scores)
    newScores(nextPlayer) += newScore
    newScores.toArray
  }

  def addMarble(m: Int): Vector[Int] = {
    val h = marbles.last
    Vector(m, h) ++ marbles.dropRight(1)
  }
}

def playGame(turn: Turn, marbleValue: Int): Turn = {
  // Add a bit of visilibity...
  marbleValue match {
    case m if m % 23 == 0 => turn.winTurn(m)
    case m => turn.classicTurn(m)
  }
}

val turn0 = Turn(-1, Vector(0), Array.fill(players)(0))
// Print board on each turn :
// (1 to lastMarble).scanLeft(turn0)(playGame).foreach(println)

val last = (1 to lastMarble).foldLeft((0L, turn0)) {
  case ((max, t), m) =>
    val newT = playGame(t, m)
    val maxScore = newT.scores.max
    (maxScore, newT)
}

println(s"Expected: ${expected}")
println(s"Result: ${last._1}")
