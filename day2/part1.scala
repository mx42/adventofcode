import scala.io.StdIn._

val res = Iterator
  .continually(readLine)
  .takeWhile(_ != null)
  .toList
  .map {
    entry => val counts = entry
      .groupBy(_.charValue)
      .mapValues(_.length)
      .values
    (counts.find(_ == 2), counts.find(_ == 3))
  }
  .foldLeft((0, 0)) {
    (acc, item) => (acc._1 + item._1.map(_ => 1).getOrElse(0),
                    acc._2 + item._2.map(_ => 1).getOrElse(0)) }

println(res._1 * res._2)
