import scala.io.Source

case class Bank(id: Int, numberOfBlocks: Int)

type Banks = Vector[Bank]

val banksConfig = {
  val src = Source.fromFile("/Users/mark.buss/Dev/algorithmic-code-club/advent-of-code/src/main/2017/day6-part1.txt")
  val line = src.getLines.take(1).toList.head
  src.close
  line
}

//val masterBanks = Vector(Bank(1, 0), Bank(2, 2), Bank(3,7), Bank(4, 0))
val masterBanks: Banks = banksConfig.split('\t').foldLeft(Vector.empty[Bank]){
  (banks, blocks) => banks.appended(Bank(banks.length+1, blocks.toInt))
}

println(masterBanks)
def highestBank(banks: Banks):Bank =
  banks.sortWith { (first, second) =>
    if (first.numberOfBlocks == second.numberOfBlocks)
      first.id < second.id
    else
      first.numberOfBlocks > second.numberOfBlocks
  }.head

def getPattern(banks: Banks): String = {
  banks.map(b => b.numberOfBlocks).mkString("|")
}

def getIndex(index: Int, length: Int): Int =
  if(index > length) {
    (index-1) % length + 1
  } else {
    index
  }

def addToBank(banks: Banks, index: Int): Banks = {
  val currentBank = banks(index-1)
  val updatedBank = Bank(currentBank.id, currentBank.numberOfBlocks + 1)
  banks.updated(index-1, updatedBank)
}

def distribute(startingBanks: Banks, highestBank: Bank) = {
  val startingIndex = getIndex(highestBank.id+1, startingBanks.length)
  val startPoint = startingBanks.updated(highestBank.id-1, Bank(highestBank.id, 0))
  (startingIndex to startingIndex+highestBank.numberOfBlocks-1).foldLeft(startPoint) { (banks, loopIndex) =>
    val index = getIndex(loopIndex, banks.length)
    val updated = addToBank(banks, index)
    updated
  }
}


def updateAndCheck(banks: Banks, prevPatterns: Map[String, Int], stepCount: Int): Int = {
  val highest = highestBank(banks)
  val updatedBanks = distribute(banks, highest)
  val newPattern = getPattern(updatedBanks)

  if(prevPatterns.contains(newPattern)) {
    println(s"Repeated pattern is $newPattern after $stepCount steps")
    prevPatterns.get(newPattern).map(firstSeenStepCount => {
      println(s"Loop size is ${stepCount-firstSeenStepCount}")
    })
    stepCount
  } else {
    println(s"Not seen pattern $newPattern yet, continuing")
    updateAndCheck(updatedBanks, prevPatterns.updated(newPattern, stepCount), stepCount + 1)
  }
}

println("Calling update and check")
val stepCount = updateAndCheck(masterBanks, Map[String, Int]((getPattern(masterBanks), 0)), 1)
println(s"Repeated pattern in $stepCount steps")
