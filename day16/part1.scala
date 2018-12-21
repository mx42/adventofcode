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

case class Instruction(opcode: Int, A: Int, B: Int, C: Int)

case class WipAnalysis(instr: Option[Instruction], before: Option[Registers], after: Option[Registers])
case class Analysis(instr: Instruction, before: Registers, after: Registers) {
  lazy val eligibleOpcodes: List[String] = OP_CODES.filter {
    case (op, f) => f(instr.A, instr.B, instr.C, before) == after
  }.keys.toList
}

val (analysisLines, programLines, _, _) = Iterator
  .continually(readLine)
  .takeWhile(_ != null)
  .foldLeft(
    (List.empty[String], List.empty[String], 0, 0)
  ) {
    case ((analysis, program, phase, blankLines), line) if line.isEmpty => (analysis, program, phase, blankLines + 1)
    case ((analysis, _, 0, _), line) => (List(line), List(), 1, 0)
    case ((analysis, _, 1, blankLines), line) if blankLines < 2 => (analysis ++ List(line), List(), 1, 0)
    case ((analysis, program, 1, blankLines), line) if blankLines >= 2 => (analysis, program ++ List(line), 2, 0)
    case ((analysis, program, 2, _), line) => (analysis, program ++ List(line), 2, 0)
  }

def parseRegs(line: String): Registers =
  line.filter(c => c.isDigit || c.isSpaceChar).trim.split(" ").map(_.toInt).toList

def parseInstr(line: String): Option[Instruction] =
  line.split(" ").map(_.toInt).toList match {
    case op :: a :: b :: c :: Nil => Some(Instruction(op, a, b, c))
    case _ => println(s"Couldnt parse [$line] ?!"); None
  }

val analysis: List[Analysis] = analysisLines
  .foldLeft( (List.empty[WipAnalysis], WipAnalysis(None, None, None), 0) ) {
    case ((acc, cur, 0), line) => (acc, cur.copy(before = Some(parseRegs(line))), 1)
    case ((acc, cur, 1), line) => (acc, cur.copy(instr = parseInstr(line)), 2)
    case ((acc, cur, 2), line) => (cur.copy(after = Some(parseRegs(line))) :: acc, WipAnalysis(None, None, None), 0)
  }._1.map(wip => Analysis(wip.instr.get, wip.before.get, wip.after.get))

println(s"Samples with 3+ eligible opcodes: ${analysis.filter(_.eligibleOpcodes.size > 2).size}")
