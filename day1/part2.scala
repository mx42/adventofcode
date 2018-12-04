/**
 * Usage: scala part2.scala < input
 */

import scala.io.StdIn
import scala.collection.Iterator

val items = Stream
  .continually(StdIn.readLine)
  .takeWhile(_ != null)
  .map(Integer.parseInt)
  .toList

println(
Stream
  .continually(items.toStream)
  .flatten
  .scanLeft(0)(_ + _)
  .scanLeft(Map.empty[Int, Int]){case (acc, item) => acc + (item -> (acc.getOrElse(item, 0) + 1)) }
  .filter(_.count(_._2 == 2) > 0)
  .map(_.maxBy(_._2))
  .head
)
