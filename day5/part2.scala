import scala.io.StdIn._

val ASCII_MIN_MAJ_INTERVAL = 32

def reactingPolymer(poly: String, unit: Char): String = unit match {
  case _ if poly.isEmpty => unit.toString
  case _ if (poly.last - unit).abs == ASCII_MIN_MAJ_INTERVAL => poly.dropRight(1)
  case _ => poly + unit
}

val polymer_v1 = readLine.foldLeft("")(reactingPolymer)
val unitTypes = polymer_v1.toLowerCase.toSet

unitTypes
  .map(u => (u, polymer_v1.filter(c => c != u && c + ASCII_MIN_MAJ_INTERVAL != u)))
  .map { case (u, poly) => (u, poly.foldLeft("")(reactingPolymer).size) }
  .toList
  .sortBy(_._2)
  .map { case (u, size) => s"Could reach size $size by removing $u" }
  .take(1)
  .foreach(println)
