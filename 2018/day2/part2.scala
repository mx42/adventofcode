import scala.io.StdIn._


val items = Iterator
  .continually(readLine)
  .takeWhile(_ != null)
  .toList

val res = items
  .combinations(2)
  .map(x => x(0) zip x(1))
  .dropWhile(_.count(t => t._1 != t._2) != 1)
  .map(_.filter(t => t._1 == t._2).map(_._1))
  .next

println(res.mkString(""))
