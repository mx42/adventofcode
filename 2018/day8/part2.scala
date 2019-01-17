import scala.io.StdIn.readLine

case class Node(c: Char, nbChild: Int, nbMetadata: Int, children: List[Node], metadata: List[Int]) {
  val metaSum: Int = metadata.sum
}

val it = readLine.split(" ").map(_.toInt).toList

def readNode(c: Char, input: List[Int]): (Int, Node) = input match {
  case nbChild :: nbMeta :: t =>
    val (read, children) = List.range(0, nbChild, 1).foldLeft((0, List.empty[Node])) { case (acc, id) =>
      val (read, node) = readNode((c + id + 1).toChar, t.drop(acc._1))
      (acc._1 + read, acc._2 ++ List(node))
    }
    val metadata: List[Int] = input.drop(read + 2).take(nbMeta).toList
  (read + 2 + nbMeta, Node(c, nbChild, nbMeta, children, metadata))
  case _ => (0, null)
}

val rootNode = readNode('A', it)._2

def sumMetadata(node: Node): Int =
  node match {
    case _ if node.nbChild > 0 => node.metadata.map {
      case m if m <= node.nbChild => sumMetadata(node.children(m - 1))
      case _ => 0
    }.sum
    case _ => node.metaSum
  }

println(sumMetadata(rootNode))
