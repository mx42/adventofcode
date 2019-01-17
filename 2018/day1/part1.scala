/**
 * Usage: scala part1.scala < input
 */

import scala.io.StdIn
import scala.collection.Iterator

println(Iterator
  .continually(StdIn.readLine)
  .takeWhile(_ != null)
  .map(Integer.parseInt)
  .sum)
