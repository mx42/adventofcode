import scala.io.StdIn.readLine
import scala.collection.mutable
import scala.annotation.tailrec

val ESC = "\u001B"
val RED = s"$ESC[1;31m"
val BLU = s"$ESC[1;34m"
val CLR = s"$ESC[0;37m"

val CLEAR = false
val CLEAR_SCREEN = s"$ESC[2J"
val SLEEP_TIME = 200

val ELF = 'E'
val GOBLIN = 'G'

case class Pos(x: Int, y: Int) extends Ordered[Pos] {
  lazy val neighbours: List[Pos] = posOrder.map(_ + this)

  def +(that: Pos): Pos = Pos(this.x + that.x, this.y + that.y)

  def compare(that: Pos): Int =
    this.y - that.y match {
      case 0 => this.x - that.x
      case ydiff => ydiff
    }

  override def toString: String = s"@${x}x${y}"
}

object Origin extends Pos(0, 0)
object Up extends Pos(0, -1)
object Down extends Pos(0, 1)
object Left extends Pos(-1, 0)
object Right extends Pos(1, 0)

val posOrder: List[Pos] = List(Up, Left, Right, Down)

sealed trait Environment
case object Wall extends Environment
case object Free extends Environment

case class Entity(p: Pos, race: Char, h: Int = 200, atk: Int = 3) extends Ordered[Entity] {
  override def toString: String = s"$race($h)"
  def compare(that: Entity): Int = this.p.compare(that.p)
}

case class Game(map: Set[Pos], startUnits: List[Entity]) {
  lazy val Pos(maxX, maxY) =
    map.foldLeft(Origin: Pos) {
      case (Pos(mx, my), Pos(x, y)) if x > mx && y > my => Pos(x, y)
      case (Pos(mx, my), Pos(x, y)) if x > mx => Pos(x, my)
      case (Pos(mx, my), Pos(x, y)) if y > my => Pos(mx, y)
      case (acc, _) => acc
    }

  lazy val getOutcome: Round = Iterator.from(2)
    .scanLeft(Round(startUnits, 1))(nextRound)
    .filter(_.gameFinished)
    .next

  def nextRound(r: Round, nb: Int): Round =
    Round(r.remainingEntities, nb) 

  def moveToClosest(entity: Entity, targetPos: Set[Pos], freeCells: Set[Pos]): Entity = entity.p match {
    case p if targetPos.contains(p) => entity
    case _ if targetPos.isEmpty => entity
    case p => val closest = closestPos(p, targetPos, freeCells).sorted.headOption
      closest.map(c => entity.copy(p = moveTowardsTarget(p, c, freeCells))).getOrElse(entity)
  }

  def moveTowardsTarget(origin: Pos, target: Pos, free: Set[Pos]): Pos =
    closestPos(target, posOrder.map(_ + origin).filter(free.contains).toSet, free).min

  def closestPos(p: Pos, targetPos: Set[Pos], freeCells: Set[Pos]): List[Pos] = {
    @tailrec
    def explore(todo: List[(Pos, Int)], free: Set[Pos], result: List[(Pos, Int)]): List[(Pos, Int)] =
      todo match {
        case Nil => result
        case (p, d) :: tail =>
          explore(
            tail ++ p.neighbours
              .filter(free.contains)
              .filterNot(q => todo.map(_._1).contains(q) || result.map(_._1).contains(q))
              .map(_ -> (d + 1)),
            free,
            (p -> d) :: result)
      }
    val distPos = explore(List(p -> 0), freeCells, List())
      .filter(q => targetPos.contains(q._1))
    distPos.filter(_._2 == distPos.map(_._2).min).map(_._1)
  }

  case class Round(units: List[Entity], roundNb: Int) {
    lazy val (remainingEntities, roundFinished, gameFinished) = getOutcome

    def getOutcome: (List[Entity], Boolean, Boolean) = {
      def processTurn(entityTodo: List[Entity], doneEntity: List[Entity]): (List[Entity], Boolean) =
        entityTodo.sorted match {
          case Nil => (doneEntity, true)
          case e :: tail => {
 //           println(s"ENTITY $e (${e.p}) TURN")
            val others = tail ++ doneEntity
            val freeCells = map.filter(p => !others.exists(_.p == p))
            val enemies = others.filter(_.race != e.race)
            
            // Pick move target
            val movedE = enemies
              .flatMap(o => o.p.neighbours)
              .filter(freeCells.contains) match {
                case Nil => e
                case ps => moveToClosest(e, ps.toSet, freeCells)
              }
//            if (movedE != e) {
//              println(s"MOVED TO ${movedE.p}")
//            }

            // Pick attack target
            val attackTarget = enemies
              .filter(o => movedE.p.neighbours.contains(o.p))
              .sortWith { case (a, b) => a.h - b.h match {
                case 0 => a.p.compare(b.p) < 0
                case hdiff => hdiff < 0
              } }
              .headOption
//            if (attackTarget.nonEmpty) {
//              println(s"ATTACK TARGET ${attackTarget.get} (@${attackTarget.get.p})")
//            }

            // Update queues
            val (updTail, updDone) = attackTarget match {
              case None => (tail, doneEntity)
              case Some(a) if a.h > e.atk =>
                val updated = a.copy(h = a.h - e.atk)
                tail match {
                  case _ if tail.contains(a) =>
                    ((updated :: tail.filterNot(_ == a)), doneEntity)
                  case _ if doneEntity.contains(a) =>
                    (tail, (updated :: doneEntity.filterNot(_ == a)))
                  case _ =>
                    println("!!!!!!!!!!!!!!!!!11")
                    println(s"Attack target : $a")
                    println(s"Tail : $tail")
                    println(s"Done Entity : $doneEntity")
                    (tail.filterNot(_ == a), doneEntity.filterNot(_ == a))
                }
              case Some(a) =>
                println(s"Entity $a died at pos ${a.p} - attacker $e (@${e.p})")
                (tail.filterNot(_ == a), doneEntity.filterNot(_ == a))
            }

            enemies match {
              case Nil => (movedE :: updTail ++ updDone, false)
              case _ => processTurn(updTail, movedE :: updDone)
            }
          }
        }

      printRound(units, roundNb)
      processTurn(units, Nil) match {
        case (u, f) if u.groupBy(_.race).size == 2 => (u, f, false) // f should be always true ?
        case (u, f) => (u, f, true)
      }
    }
 
    def printRound(units: List[Entity], roundNb: Int): Unit =
      if (CLEAR) {
        println(CLEAR_SCREEN)
      }
      println(
        (s"Round $roundNb" ::
         ((0 to maxY) map {
           y =>
             val lb = mutable.ListBuffer.empty[Entity]
             val chars = (0 to maxX) map {
               x => val p = Pos(x, y)
               (map.contains(p), units.find(_.p == p)) match {
                 case (_, Some(e: Entity)) if e.race == ELF => lb += e; BLU + e.race + CLR
                 case (_, Some(e: Entity)) if e.race != ELF => lb += e; RED + e.race + CLR
                 case (false, _) => "#"
                 case (true, _) => "."
               }
             }
           chars.mkString("") + " " + lb.mkString(", ")
         }).toList
       ).mkString("\n"))
    if (CLEAR) {
      Thread.sleep(SLEEP_TIME)
    }
  }
}

val input = Iterator
  .continually(readLine)
  .takeWhile(_ != null)
  .toList
  .zipWithIndex
  .flatMap {
    case (l, y) => l.zipWithIndex.map {
      case (c, x) => Pos(x, y) -> c
    } 
  }
  .flatMap {
    case (p, '#') => List()
    case (p, '.') => List(p -> Free)
    case (p, 'E') => List(p -> Entity(p, ELF), p -> Free)
    case (p, 'G') => List(p -> Entity(p, GOBLIN), p -> Free)
  }

val gameMap: Set[Pos] = input
  .flatMap {
    case (k: Pos, v: Environment) => Some(k)
    case _ => None
  }.toSet

val units = input
  .flatMap {
    case (p: Pos, v: Entity) => Some(v)
    case _ => None
  }

val lastRound = Game(gameMap, units).getOutcome

val lastRoundId = if (lastRound.roundFinished) {
  lastRound.roundNb
} else {
  lastRound.roundNb - 1
}

val remainingUnitsHP = lastRound.remainingEntities.map(_.h).sum

println(s"Last round nb: ${lastRound.roundNb}")
println(s"Last complete round: ${lastRoundId}")
println(s"Remaining units cumulative HP: ${remainingUnitsHP}")

println(s"Final score ${lastRoundId * remainingUnitsHP}")
