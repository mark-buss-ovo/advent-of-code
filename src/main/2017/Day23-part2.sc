import scala.io.Source
import scala.util.matching.Regex

val bufferedSource = Source.fromFile("/Users/mark.buss/Dev/algorithmic-code-club/advent-of-code/src/main/2017/Day23-input.txt")

val i: Int = 5
val l: Long = 10

l - i

trait Instruction {
  def run(registers: Map[Char, Long]): (Map[Char, Long], Int)
}

case class SetFromValue(index: Int, registerId: Char, value: Long) extends Instruction {
  override def run(registers: Map[Char, Long]): (Map[Char, Long], Int) = {
    (registers.updated(registerId, value), index + 1)
  }
}

case class SetFromRegister(index: Int, destRegisterId: Char, sourceRegisterId: Char) extends Instruction {
  override def run(registers: Map[Char, Long]): (Map[Char, Long], Int) = {
    (registers.updated(destRegisterId, registers.getOrElse(sourceRegisterId, 0L)), index + 1)
  }
}

case class SubWithValue(index: Int, registerId: Char, value: Int) extends Instruction {
  override def run(registers: Map[Char, Long]): (Map[Char, Long], Int) = {
    val currentValue = registers.getOrElse(registerId, 0L)
    val newValue = currentValue - value
    (registers.updated(registerId, newValue), index + 1)
  }
}

case class SubWithRegister(index: Int, destRegisterId: Char, sourceRegisterId: Char) extends Instruction {
  override def run(registers: Map[Char, Long]): (Map[Char, Long], Int) = {
    (registers.updated(destRegisterId, registers.getOrElse(destRegisterId, 0L) - registers.getOrElse(sourceRegisterId, 0L)), index + 1)
  }
}

case class MulWithValue(index: Int, registerId: Char, value: Int) extends Instruction {
  override def run(registers: Map[Char, Long]): (Map[Char, Long], Int) = {
    (registers.updated(registerId, registers.getOrElse(registerId, 0L) * value), index + 1)
  }
}

case class MulWithRegister(index: Int, destRegisterId: Char, sourceRegisterId: Char) extends Instruction {
  override def run(registers: Map[Char, Long]): (Map[Char, Long], Int) = {
    (registers.updated(destRegisterId, registers.getOrElse(destRegisterId, 0L) * registers.getOrElse(sourceRegisterId, 0L)), index + 1)
  }
}

case class JumpFromValue(index: Int, checkValue: Int, jumpSize: Int) extends Instruction {
  override def run(registers: Map[Char, Long]): (Map[Char, Long], Int) = {
    (registers, if(checkValue == 0) index + 1 else index + jumpSize)
  }
}

case class JumpFromRegister(index: Int, registerId: Char, jumpSize: Int) extends Instruction {
  override def run(registers: Map[Char, Long]): (Map[Char, Long], Int) = {
    (registers, if(registers.getOrElse(registerId, 0L) == 0L) index + 1 else index + jumpSize)
  }
}


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
    val value1 = matches.group(2).charAt(0)
    val value2 = matches.group(4)

    val instruction: Instruction = operation match {
      case "set" =>
        value2.toIntOption.fold[Instruction](SetFromRegister(instructionList.length, value1, value2.charAt(0)))(SetFromValue(instructionList.length, value1, _))
      case "sub" =>
        value2.toIntOption.fold[Instruction](SubWithRegister(instructionList.length, value1, value2.charAt(0)))(SubWithValue(instructionList.length, value1, _))
      case "mul" =>
        value2.toIntOption.fold[Instruction](MulWithRegister(instructionList.length, value1, value2.charAt(0)))(MulWithValue(instructionList.length, value1, _))
      case "jnz" =>
        if(value1.isDigit) JumpFromValue(instructionList.length, value1.asDigit, value2.toInt) else JumpFromRegister(instructionList.length, value1, value2.toInt)
    }

    instructionList.appended(instruction)
  }
})

println(instructions)

def process(registers: Map[Char, Long], index: Int, runCount: Int): Long = {

    if(runCount == 2000000 || index < 0 || index > instructions.length - 1) {
      registers.getOrElse('h', 0L)
    }
    else {
      val instruction = instructions(index)
      //println(index, instruction)
      val (updatedRegisters, nextIndex) = instruction.run(registers)
      if(index == 19) println(updatedRegisters)
      process(updatedRegisters, nextIndex, runCount + 1)
    }
}

process(Map('a' -> 1L), 0, 0)


