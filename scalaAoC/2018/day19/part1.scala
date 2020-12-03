import scala.io.StdIn.readLine

def ifb(A: Int, B: Int, f: (Int, Int) => Boolean): Int = if (f(A, B)) {
  1
} else {
  0
}

val OP_CODES: Map[String, ((Int, Int, Int, Registers) => Registers)] = Map(
  "addr" -> ((A: Int, B: Int, C: Int, regs: List[Int]) => regs.patch(C, List(regs(A) + regs(B)), 1)),
  "addi" -> ((A: Int, B: Int, C: Int, regs: List[Int]) => regs.patch(C, List(regs(A) + B), 1)),

  "mulr" -> ((A: Int, B: Int, C: Int, regs: List[Int]) => regs.patch(C, List(regs(A) * regs(B)), 1)),
  "muli" -> ((A: Int, B: Int, C: Int, regs: List[Int]) => regs.patch(C, List(regs(A) * B), 1)),

  "banr" -> ((A: Int, B: Int, C: Int, regs: List[Int]) => regs.patch(C, List(regs(A) & regs(B)), 1)),
  "bani" -> ((A: Int, B: Int, C: Int, regs: List[Int]) => regs.patch(C, List(regs(A) & B), 1)),

  "borr" -> ((A: Int, B: Int, C: Int, regs: List[Int]) => regs.patch(C, List(regs(A) | regs(B)), 1)),
  "bori" -> ((A: Int, B: Int, C: Int, regs: List[Int]) => regs.patch(C, List(regs(A) | B), 1)),

  "setr" -> ((A: Int, B: Int, C: Int, regs: List[Int]) => regs.patch(C, List(regs(A)), 1)),
  "seti" -> ((A: Int, B: Int, C: Int, regs: List[Int]) => regs.patch(C, List(A), 1)),

  "gtir" -> ((A: Int, B: Int, C: Int, regs: List[Int]) => regs.patch(C, List(ifb(A, regs(B), _ > _)), 1)),
  "gtri" -> ((A: Int, B: Int, C: Int, regs: List[Int]) => regs.patch(C, List(ifb(regs(A), B, _ > _)), 1)),
  "gtrr" -> ((A: Int, B: Int, C: Int, regs: List[Int]) => regs.patch(C, List(ifb(regs(A), regs(B), _ > _)), 1)),

  "eqir" -> ((A: Int, B: Int, C: Int, regs: List[Int]) => regs.patch(C, List(ifb(A, regs(B), _ == _)), 1)),
  "eqri" -> ((A: Int, B: Int, C: Int, regs: List[Int]) => regs.patch(C, List(ifb(regs(A), B, _ == _)), 1)),
  "eqrr" -> ((A: Int, B: Int, C: Int, regs: List[Int]) => regs.patch(C, List(ifb(regs(A), regs(B), _ == _)), 1))
)

type Registers = List[Int]

case class Instruction(opcode: String, A: Int, B: Int, C: Int) {
  override def toString: String = s"$opcode $A $B $C"
}

object Instruction {
  def apply(str: String): Instruction = str.split(" ").toList match {
    case op :: a :: b :: c :: Nil => Instruction(op, a.toInt, b.toInt, c.toInt)
    case _ => throw new RuntimeException("Invalid input")
  }
}

val (Some(ptr), programLines) = Iterator
  .continually(readLine)
  .takeWhile(_ != null)
  .foldLeft(
    (None : Option[Int], List.empty[Instruction])
  ) {
    case ((None, _), line) => (Some(line.split(" ")(1).toInt), List())
    case ((ptr, lines), line) => (ptr, lines ++ List(Instruction(line)))
}

def executeProgram(ptr: Int, instructions: List[Instruction], regs: Registers): Registers = regs(ptr) match {
  case p if instructions.isDefinedAt(p) =>
    val inst = instructions(p)
    val outputRegs = OP_CODES(inst.opcode)(inst.A, inst.B, inst.C, regs)
    val incrPtrInRegs = outputRegs.patch(ptr, List(outputRegs(ptr) + 1), 1)
  // println(s"ip=$ptr ${regs.mkString("[", ", ", "]")} $inst ${incrPtrInRegs.mkString("[", ", ", "]")}")
    executeProgram(ptr, instructions, incrPtrInRegs)
  case _ => regs
}

val endRegs = executeProgram(ptr, programLines, List.fill(6)(0))
println(s"End value in reg 0: " + endRegs(0))
