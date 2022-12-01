import scala.collection.immutable.Queue
import scala.io.Source
import scala.util.matching.Regex

val bufferedSource = Source.fromFile("/Users/mark.buss/Dev/algorithmic-code-club/advent-of-code/src/main/2017/Day18-input.txt")

case class Instruction(operation: String, value1: String, value2: Option[String])

val instructionRegex: Regex = "([a-z]{3}) ([a-z]|\\d)( (-?\\d+|[a-z])+)?".r

val instructions = bufferedSource.getLines.foldLeft(Vector.empty[Instruction])((instructionList, line) => {
  if (line.isEmpty) {
    instructionList
  }
  else {
    //println(line)
    val instructionParts = instructionRegex.findAllMatchIn(line)
    val matches = instructionParts.toList.head
    val operation = matches.group(1)
    val value1 = matches.group(2)
    val value2 = Option(matches.group(4))
    instructionList.appended(Instruction(operation, value1, value2.map(v => v.trim())))
  }
})

//println(instructions)

case class Program(id: Int, currentIndex: Integer, queue: Queue[Long], registers: Map[String, Long], waiting: Boolean, sendCount: Int)

val programsStartPoint: Map[Int, Program] = Map(
  (0, Program(0, 0, Queue.empty, Map(("p", 0)), false, 0)),
  (1, Program(1, 0, Queue.empty, Map(("p", 1)), false, 0))
)

def getValue(registerIdOrValue: String, registers: Map[String, Long]): Long = {
  registerIdOrValue.toIntOption match {
    case None => registers.getOrElse(registerIdOrValue, 0L)
    case Some(num) => num
  }
}

def performCalculationAndUpdatePrograms(instruction: Instruction,
                                        currentProgram: Program,
                                        programs: Map[Int, Program],
                                        f: (Long, Long) => Long) = {
  val toAdd = getValue(instruction.value2.get, currentProgram.registers)
  val existingValue = currentProgram.registers.getOrElse(instruction.value1, 0L)
  val newValue = f(toAdd, existingValue)
  val newRegisters = currentProgram.registers.updated(instruction.value1, newValue)
  programs.updated(currentProgram.id, currentProgram.copy(currentIndex = currentProgram.currentIndex + 1, registers = newRegisters))
}

def iterate(programId: Int, programs: Map[Int, Program], iterationCount: Int): Int = {

  val deadlocked = programs.values.forall(p => p.waiting && p.queue.isEmpty)
  if (deadlocked) {
    programs(1).sendCount
  } else {
    val otherProgramId = if (programId == 0) 1 else 0
    val currentProgram = programs(programId)
    val instruction = instructions(currentProgram.currentIndex)
    instruction.operation match {
      case "snd" =>
        val valueToSend = getValue(instruction.value1, currentProgram.registers)
        val targetProgram = programs(otherProgramId)
        val newTargetProgram = targetProgram.copy(queue = targetProgram.queue.appended(valueToSend))
        val newPrograms = Map(
          (programId, currentProgram.copy(currentIndex = currentProgram.currentIndex + 1, sendCount = currentProgram.sendCount + 1)),
          (otherProgramId, newTargetProgram)
        )
        iterate(programId, newPrograms, iterationCount + 1)
      case "set" =>
        val newValue = getValue(instruction.value2.get, currentProgram.registers)
        val newRegisters = currentProgram.registers.updated(instruction.value1, newValue)
        val newPrograms = programs.updated(programId, currentProgram.copy(currentIndex = currentProgram.currentIndex + 1, registers = newRegisters))
        iterate(programId, newPrograms, iterationCount + 1)
      case "add" =>
        val newPrograms = performCalculationAndUpdatePrograms(instruction, currentProgram, programs, _+_)
        iterate(programId, newPrograms, iterationCount + 1)
      case "mul" =>
        val newPrograms = performCalculationAndUpdatePrograms(instruction, currentProgram, programs, _ * _)
        iterate(programId, newPrograms, iterationCount + 1)
      case "mod" =>
        val newPrograms = performCalculationAndUpdatePrograms(instruction, currentProgram, programs, (op1, op2) => op2 % op1)
        iterate(programId, newPrograms, iterationCount + 1)
      case "rcv" =>
        if (currentProgram.queue.isEmpty) {
          val newPrograms = programs.updated(programId, currentProgram.copy(waiting = true))
          iterate(otherProgramId, newPrograms, iterationCount + 1)
        } else {
          val (value, newQueue) = currentProgram.queue.dequeue
          val newRegisters = currentProgram.registers.updated(instruction.value1, value)
          val newPrograms = programs.updated(programId, currentProgram.copy(currentIndex = currentProgram.currentIndex + 1, queue = newQueue, registers = newRegisters, waiting = false))
          iterate(programId, newPrograms, iterationCount + 1)
        }
      case "jgz" =>
        val checkValue = getValue(instruction.value1, currentProgram.registers)
        val value = getValue(instruction.value2.get, currentProgram.registers)
        val nextIndex = checkValue match {
          case v if v > 0 => (currentProgram.currentIndex + value).toInt
          case _ => currentProgram.currentIndex + 1
        }
        val newPrograms = programs.updated(programId, currentProgram.copy(currentIndex = nextIndex))
        iterate(programId, newPrograms, iterationCount + 1)

    }

  }
}

iterate(0, programsStartPoint, 1)


