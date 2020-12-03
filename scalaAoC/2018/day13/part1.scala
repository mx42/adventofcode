import scala.io.StdIn.readLine
import scala.annotation.tailrec

val ESC = "\u001B"
val RED = s"$ESC[1;31m"
val BLU = s"$ESC[1;34m"
val CLR = s"$ESC[0;37m"

val CLEAR_SCREEN = s"$ESC[2J"

case class Pos(x: Int, y: Int) {
  def +(that: Pos): Pos = Pos(x + that.x, y + that.y)
}

object Up extends Pos(0, -1)
object Down extends Pos(0, 1)
object Left extends Pos(-1, 0)
object Right extends Pos(1, 0)

val directions: Array[Pos] = Array(
  Left,
  Up,
  Right,
  Down)

sealed trait TrackOrientation {
  val c: Char
  def move(cart: Cart): Cart
}

trait Straight extends TrackOrientation {
  override def move(cart: Cart): Cart =
    cart.copy(p = cart.p + cart.o)
}

case object StraightImpl extends Straight {
  override val c: Char = '?'
}

case object Horizontal extends Straight {
  override val c: Char = '-'
}
case object Vertical extends Straight {
  override val c: Char = '|'
}

case object Diag1 extends TrackOrientation {
  override val c: Char = '/'
  override def move(cart: Cart): Cart = cart.o match {
    case Up => TurnRight.move(cart)
    case Down => TurnRight.move(cart)
    case Left => TurnLeft.move(cart)
    case Right => TurnLeft.move(cart)
  }
}

case object Diag2 extends TrackOrientation {
  override val c: Char = '\\'
  override def move(cart: Cart): Cart = cart.o match {
    case Up => TurnLeft.move(cart)
    case Down => TurnLeft.move(cart)
    case Left => TurnRight.move(cart)
    case Right => TurnRight.move(cart)
  }
}

case object TurnLeft extends TrackOrientation {
  override val c: Char = '?'

  val changes: Map[Pos, Pos] = Map(
    Up -> Left,
    Down -> Right,
    Left -> Down,
    Right -> Up
  )

  override def move(cart: Cart): Cart = {
    val newDir = changes(cart.o)
    cart.copy(o = newDir, p = cart.p + newDir)
  }
}

case object TurnRight extends TrackOrientation {
  override val c: Char = '?'

  val changes: Map[Pos, Pos] = Map(
    Up -> Right,
    Down -> Left,
    Left -> Up,
    Right -> Down
  )

  override def move(cart: Cart): Cart = {
    val newDir = changes(cart.o)
    cart.copy(o = newDir, p = cart.p + newDir)
  }
}

val crossDirections = Array(
  TurnLeft,
  StraightImpl,
  TurnRight)

case object CX extends TrackOrientation {
  override val c: Char = '+'

  override def move(cart: Cart): Cart =
    crossDirections(cart.nextCross)
      .move(cart.copy(nextCross=(cart.nextCross + 1) % 3))
}

sealed trait Entry

case class Track(p: Pos, o: TrackOrientation) extends Entry

case class Cart(p: Pos, o: Pos, nextCross: Int = 0) extends Entry with Ordered[Cart] {
  def compare(that: Cart): Int = {
    val cmpL = p.y - that.p.y
    if (cmpL == 0) {
      p.x - that.p.x
    } else {
      cmpL
    }
  }

  def next(tracks: Map[Pos, TrackOrientation]): Cart =
    tracks(p).move(this)

  override def toString: String = "#"
}

val input = Iterator
  .continually(readLine)
  .takeWhile(_ != null)
  .toList
  .zipWithIndex
  .flatMap { case (l, y) => l.zipWithIndex.map { case (c, x) => Pos(x, y) -> c } }
  .flatMap {
    // tracks
    case (p, c) if c == '|' => List(Track(p, Vertical))
    case (p, c) if c == '-' => List(Track(p, Horizontal))
    case (p, c) if c == '+' => List(Track(p, CX))
    case (p, c) if c == '\\' => List(Track(p, Diag2))
    case (p, c) if c == '/' => List(Track(p, Diag1))
    case (p, c) if c == 'v' => List(Track(p, Vertical), Cart(p, Down))
    case (p, c) if c == '^' => List(Track(p, Vertical), Cart(p, Up))
    case (p, c) if c == '>' => List(Track(p, Horizontal), Cart(p, Right))
    case (p, c) if c == '<' => List(Track(p, Horizontal), Cart(p, Left))
    case _ => List()
  }

val tracks: Map[Pos, TrackOrientation] = input.flatMap {
  case t: Track => Some(t.p -> t.o)
  case _ => None
}.toMap

val initialCarts: List[Cart] = input.filter(_.isInstanceOf[Cart]).map(_.asInstanceOf[Cart])

def printGame(tracks: Map[Pos, TrackOrientation], carts: List[Cart], turn: Int): Unit = {
  val maxX = tracks.keys.map(_.x).max
  val maxY = tracks.keys.map(_.y).max

  // print(CLEAR_SCREEN)

  val lines = (0 to maxY).map { y =>
    val line = (0 to maxX).map { x =>
      Pos(x, y) match {
        case p if carts.exists(_.p == p) => 
          carts.filter(_.p == p) match {
            case cs if cs.size > 1 => s"${RED}X${CLR}"
            case c :: Nil => s"${BLU}$c${CLR}"
          }
        case p if tracks.isDefinedAt(p) => tracks(p).c.toString
        case _ => " "
      }
                              }.mkString("")
                               "%03d %s".format(turn, line)
                             }
  lines.foreach(println)
  // Thread.sleep(200)
}

@tailrec
def playGame(tracks: Map[Pos, TrackOrientation])(carts: List[Cart], turn: Int): (Int, Pos) = {
  val nextCarts = carts.sorted.map(_.next(tracks))
  // printGame(tracks, nextCarts, turn)

  (nextCarts ++ carts).groupBy(_.p).find(_._2.size > 1) match {
    case Some((p, _)) => turn -> p
    case _ => playGame(tracks)(nextCarts, turn + 1)
  }
}

// printGame(tracks, initialCarts, 1)

val (crashTurn, crashPos) = playGame(tracks)(initialCarts, 2)
println(crashTurn, crashPos)
