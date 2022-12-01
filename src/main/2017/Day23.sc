import scala.io.Source
import scala.util.matching.Regex

val bufferedSource = Source.fromFile("/Users/mark.buss/Dev/algorithmic-code-club/advent-of-code/src/main/2017/Day23-input.txt")

case class Instruction(operation:String, value1: String, value2: Option[String])
val instructionRegex: Regex = "([a-z]{3}) ([a-z]|\\d)( (-?\\d+|[a-z])+)?".r

val instructions = bufferedSource.getLines.foldLeft(Vector.empty[Instruction])((instructionList, line) => {
  if(line.isEmpty) {
    instructionList
  }
  else {
    println(line)
    val instructionParts = instructionRegex.findAllMatchIn(line)
    val matches = instructionParts.toList.head
    val operation = matches.group(1)
    val value1 = matches.group(2)
    val value2 = Option(matches.group(4))
    instructionList.appended(Instruction(operation, value1, value2.map(v => v.trim())))
  }
})

println(instructions)

def getValue(registerIdOrValue: String, registers: Map[String, Long]): Long = {
  registerIdOrValue.toIntOption match {
    case None => registers.getOrElse(registerIdOrValue, 0L)
    case Some(num) => num
  }
}

def performCalculationAndUpdateRegisters(instruction: Instruction, registers: Map[String, Long], f: (Long, Long) => Long) ={
  val instructionValue = getValue(instruction.value2.get, registers)
  val existingValue = registers.getOrElse(instruction.value1, 0L)
  val newValue = f(instructionValue, existingValue)
  registers.updated(instruction.value1, newValue)
}

def process(registers: Map[String, Long], index: Int, mulCount: Int, runCount: Int): Int = {

    if(index < 0 || index > instructions.length - 1) {
      mulCount
    }
    else {
      val instruction = instructions(index)

//      println(registers.mkString("\n"))
//      println(index)
//      println(instruction)

      instruction.operation match {
        case "set" =>
          val newValue = getValue(instruction.value2.get, registers)
          val newRegisters = registers.updated(instruction.value1, newValue)
          process(newRegisters, index + 1, mulCount, runCount + 1)
        case "sub" =>
          val newRegisters = performCalculationAndUpdateRegisters(instruction, registers, (v2, v1) => v1 - v2)
          process(newRegisters, index + 1, mulCount, runCount + 1)
        case "mul" =>
          val newRegisters = performCalculationAndUpdateRegisters(instruction, registers, _ * _)
          process(newRegisters, index + 1, mulCount + 1, runCount + 1)
        case "jnz" =>
          val checkValue = getValue(instruction.value1, registers)
          val nextIndex = checkValue match {
            case v if v != 0 => (index + getValue(instruction.value2.get, registers)).toInt
            case _ => index + 1
          }
          process(registers, nextIndex, mulCount, runCount + 1)
      }
    }
}

process(Map("a" -> 1), 0, 0, 0)


