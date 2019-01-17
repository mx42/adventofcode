import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.collection.mutable

/* Used https://enear.github.io/2016/03/31/parser-combinators/ for reference. */

case class Pos(x: Int, y: Int) {
  def +(that: Pos): Pos = Pos(this.x + that.x, this.y + that.y)
  def *(m: Int): Pos = Pos(this.x * m, this.y * m)
  lazy val up: Pos = this + UpDir
  lazy val down: Pos = this + DownDir
  lazy val left: Pos = this + LeftDir
  lazy val right: Pos = this + RightDir

  def neighborsIn(ps: List[Pos]): List[Pos] = List(up, down, left, right).filter(ps.contains)
}

object UpDir extends Pos(0, -1)
object DownDir extends Pos(0, 1)
object LeftDir extends Pos(-1, 0)
object RightDir extends Pos(1, 0)
object Origin extends Pos(0, 0)

sealed trait RouteToken
case object NORTH extends RouteToken
case object SOUTH extends RouteToken
case object WEST extends RouteToken
case object EAST extends RouteToken
case object FORK_START extends RouteToken
case object FORK_END extends RouteToken
case object FORK_SEP extends RouteToken

sealed trait RouteAST
case class Route(path: Seq[RouteAST]) extends RouteAST
case class RouteFork2(pathA: Route, pathB: Route) extends RouteAST
case class RouteFork3(pathA: Route, pathB: Route, pathC: Route) extends RouteAST

sealed trait RouteDirectionAST extends RouteAST {
  val dir: Pos
}
case object NorthRoute extends RouteDirectionAST {
  override val dir = UpDir
}
case object SouthRoute extends RouteDirectionAST {
  override val dir = DownDir
}
case object WestRoute extends RouteDirectionAST {
  override val dir = LeftDir
}
case object EastRoute extends RouteDirectionAST {
  override val dir = RightDir
}

sealed trait RouteCompilerError
case class RouteLexerError(msg: String) extends RouteCompilerError
case class RouteParserError(msg: String) extends RouteCompilerError

object RouteLexer extends RegexParsers {
  def north = "N" ^^ { _ => NORTH }
  def south = "S" ^^ { _ => SOUTH }
  def west  = "W" ^^ { _ => WEST }
  def east  = "E" ^^ { _ => EAST }
  def fork_s = "(" ^^ { _ => FORK_START }
  def fork_e = ")" ^^ { _ => FORK_END }
  def fork_sep = "|" ^^ { _ => FORK_SEP }

  def tokens: Parser[List[RouteToken]] = phrase(rep1(north | south | west | east | fork_s | fork_e | fork_sep))

  def apply(input: String): Either[RouteLexerError, List[RouteToken]] = parse(tokens, input) match {
    case NoSuccess(msg, _) =>
      println(msg)
      Left(RouteLexerError(msg))
    case Success(result, _) => Right(result)
  }
}

object RouteParser extends Parsers {
  override type Elem = RouteToken

  class RouteTokenReader(tokens: Seq[RouteToken]) extends Reader[RouteToken] {
    override def first: RouteToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[RouteToken] = new RouteTokenReader(tokens.tail)
  }

  def apply(tokens: Seq[RouteToken]): Either[RouteParserError, RouteAST] = route(new RouteTokenReader(tokens)) match {
    case NoSuccess(msg, _) =>
      println(msg)
      Left(RouteParserError(msg))
    case Success(result, _) => Right(result)
  }

  def north = NORTH ^^ { _ => NorthRoute }
  def south = SOUTH ^^ { _ => SouthRoute }
  def west = WEST ^^ { _ => WestRoute }
  def east = EAST ^^ { _ => EastRoute }

  // Okay, crappy...
  def fork2: Parser[RouteAST] = FORK_START ~ rep(routePart) ~ FORK_SEP ~ rep(routePart) ~ FORK_END ^^ {
    case _ ~ route1 ~ _ ~ route2 ~ _ => RouteFork2(Route(route1), Route(route2))
  }

  def fork3: Parser[RouteAST] = FORK_START ~ rep(routePart) ~ FORK_SEP ~ rep(routePart) ~ FORK_SEP ~ rep(routePart) ~ FORK_END ^^ {
    case _ ~ route1 ~ _ ~ route2 ~ _ ~ route3 ~ _ => RouteFork3(Route(route1), Route(route2), Route(route3))
  }

  def routePart: Parser[RouteAST] = north | south | west | east | fork2 | fork3

  def route: Parser[RouteAST] = rep1(routePart) ^^ { rte => Route(rte) }
}

object RouteCompiler {
  def apply(input: String): Either[RouteCompilerError, RouteAST] = for {
    tokens <- RouteLexer(input).right
    ast <- RouteParser(tokens).right
  } yield ast
}

val roomsAndDoors: mutable.Set[Pos] = mutable.Set(Origin)

def openDoor(pos: Pos, direction: RouteDirectionAST): Pos = {
  val door = pos + direction.dir
  val newPos = door + direction.dir

  roomsAndDoors += door
  roomsAndDoors += newPos

  newPos
}

def mapDoors(route: Route, curPos: Pos): Unit = route.path match {
  case Nil => 
  case RouteFork2(pathA, pathB) :: t =>
    mapDoors(pathA, curPos)
    mapDoors(pathB, curPos)
    mapDoors(Route(t), curPos)
  case RouteFork3(pathA, pathB, pathC) :: t =>
    mapDoors(pathA, curPos)
    mapDoors(pathB, curPos)
    mapDoors(pathC, curPos)
    mapDoors(Route(t), curPos)
  case (d: RouteDirectionAST) :: t =>
    mapDoors(Route(t), openDoor(curPos, d))
}

def getMinMax(ps: Iterable[Pos]): (Pos, Pos) = {
  val xs = ps.map(_.x)
  val ys = ps.map(_.y)
  (Pos(xs.min - 1, ys.min - 1), Pos(xs.max + 1, ys.max + 1))
}

def printMap(min: Pos, max: Pos, rooms: Set[Pos]): String = {
  val lines = (min.y to max.y) map {
    y => val line = (min.x to max.x) map {
      x: Int => Pos(x, y) match {
        case Origin => "O"
        case p if rooms.contains(p) => " "
        case _ => "#"
      }
    }
    line.mkString("")
  }
  lines.mkString("\n")
}

val rawInput = scala.io.StdIn.readLine
// println(rawInput)

val input = rawInput match {
  case l => RouteCompiler(l.slice(1, l.size - 1)).right.get 
}

// println(input)

mapDoors(input.asInstanceOf[Route], Origin)
val (min, max) = getMinMax(roomsAndDoors)

// println(s"Min coords: $min\nMax coords: $max")

println(printMap(min, max, roomsAndDoors.toSet))

def computeDists(remaining: List[Pos], todo: List[(Pos, Int)], distMap: Map[Pos, Int]): Map[Pos, Int] = todo match {
  case Nil => distMap
  case (node, dist) :: tail =>
    val neighbors = node.neighborsIn(remaining)
    computeDists(remaining.filterNot(neighbors.contains), tail ++ neighbors.map(p => (p, dist + 1)), distMap + (node -> dist))
}

val distMap = computeDists(roomsAndDoors.toList, List((Origin -> 0)), Map.empty[Pos, Int])

println("Max doors to pass: " + distMap.map(_._2).max / 2)
