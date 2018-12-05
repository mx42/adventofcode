import scala.io.StdIn.readLine
import java.time.{LocalDate, LocalTime}
import java.time.format.DateTimeFormatter

val fmt: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd kk:mm")

case class Entry(shiftDate: LocalDate, time: LocalTime, id: Option[Int], sleep: Option[Boolean])

def parseEntry(input: String): Entry = {
  "\\[([^\\]]+)\\] ([^ ]+) ([^ ]+)".r.findFirstMatchIn(input).map {
    m => 
      val ts = fmt.parse(m.group(1))
      val time = LocalTime.from(ts)
      val shiftDate = time match {
        case t if t.getHour == 23 => LocalDate.from(ts).plusDays(1L)
        case _ => LocalDate.from(ts)
      }

      m.group(2) match {
      case "Guard" => Entry(shiftDate, time, Some(m.group(3).drop(1).toInt), None)
      case "falls" => Entry(shiftDate, time, None, Some(true))
      case "wakes" => Entry(shiftDate, time, None, Some(false))
    }
  }.get
}

case class Sleep(id: Int, begin: LocalTime, end: LocalTime)

def buildSleeps(input: List[Entry]): List[Sleep] = {
  val guardNb = input.filter(_.id.nonEmpty).map(_.id.get).head
  val sleepCycles = input.filter(e => e.sleep.nonEmpty).map(e => e.time -> e.sleep.get).sortBy(_._1.toSecondOfDay)
  
  (sleepCycles.filter(_._2).map(_._1) zip sleepCycles.filterNot(_._2).map(_._1))
    .map(s => Sleep(guardNb, s._1, s._2))
}

val entries = Iterator
  .continually(readLine)
  .takeWhile(_ != null)
  .map(parseEntry)
  .toList

val sleeps = entries.groupBy(_.shiftDate)
  .flatMap { case (k, v) => buildSleeps(v) }

val sleepers = sleeps
  .groupBy(_.id).toList
  .sortBy { case (k, v) => v.map(s => s.end.toSecondOfDay - s.begin.toSecondOfDay).sum }
  .reverse

val guardId = sleepers.head._1

println(s"Guard ID: $guardId")

val mostAsleepMinute = sleepers.head._2
  .flatMap(s => s.begin.getMinute.to(s.end.getMinute - 1))
  .groupBy(identity)
  .mapValues(_.size)
  .maxBy(_._2)
  ._1

println(s"Most asleep minute: $mostAsleepMinute")

println(s"Result: ${guardId * mostAsleepMinute}")
