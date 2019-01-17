import scala.io.StdIn._

val ASCII_MIN_MAJ_INTERVAL = 32 // 'a' - 'A'

val res = readLine
  .foldLeft(""){
    case (acc, chr) => chr match {
      case _ if acc.isEmpty => chr.toString
      case _ if (acc.last - chr).abs == ASCII_MIN_MAJ_INTERVAL => acc.dropRight(1)
      case _ => acc + chr
    }
  }

println(res.size)
