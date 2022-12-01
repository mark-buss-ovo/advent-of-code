import scala.io.Source
import scala.util.matching.Regex

val bufferedSource = Source.fromFile(
  "/Users/mark.buss/Dev/algorithmic-code-club/advent-of-code/src/main/2017/Day7-input.txt"
)

sealed trait Tower
final case class SubTower(root: Program, toppers: List[Tower]) extends Tower
final case class Program(id: String, weight: Int) extends Tower

case class Tower(root: Program, children: List[Tower])
val programPartsRegex: Regex = "([a-z]+) \\((\\d+)\\)".r

val programMap =
  bufferedSource.getLines.foldLeft(Map.empty[Program, List[String]]) {
    (map, line) =>
      {
        val parts = line.split("->").map(_.trim)

        val programParts = programPartsRegex.findAllMatchIn(parts(0))
        val matches = programParts.toList.head
        val id = matches.group(1)
        val weight = matches.group(2).toInt
        val program = Program(id, weight)

        // Just the program
        if (parts.length == 1) {
          map + (program -> List.empty[String])
        } else {
          // Program with children
          val children = parts(1).split(",").map(_.trim)
          map + (program -> children.toList)
        }
      }

  }

bufferedSource.close
//part 1
val bottomProgram = programMap.keys
  .filter(program => {
    !programMap.values.toList.exists(children => children.contains(program.id))
  })
  .toList
  .head
println(bottomProgram)

//part2

def getWeight(root: Program): Int = {
  val children = programMap(root)

  if (children.isEmpty) {
    root.weight
  } else {
    val programs = children.flatMap(c => programMap.keys.find(k => k.id == c))
    root.weight + programs.foldLeft(0)((acc, p) => acc + getWeight(p))
  }
}

def checkDiskBalances(root: Program, targetWeight: Int): Unit = {
  val siblingIds = programMap(root)

  val programs = siblingIds.flatMap(c => programMap.keys.find(k => k.id == c))
  println("Siblings", programs)
  val weights = programs.map(p => (p, getWeight(p)))
  println(weights)
  val groupedWeights = weights
    .groupBy { case (_, weight) => weight }

  val oddOneOut = groupedWeights.find {
    case (_, g) if g.length == 1 => true
    case _ => false
  }

  oddOneOut match {
    case None =>
      val childWeight = weights.map { case (_, w) => w }.sum

      val difference = targetWeight - childWeight
      println(childWeight, root.weight, difference, targetWeight)
      println("Correct weight", difference)
    case Some(odd) =>
      val list = odd match {
        case (_, l) => l
      }
      val nextProgram = list.head match {
        case (p, _) => p
      }

      val nextTargetWeight = groupedWeights
        .filter { case (_, l) =>
          l.length > 1
        }
        .map { case (w, _) =>
          w
        }
        .head

      checkDiskBalances(nextProgram, nextTargetWeight)
  }
}

checkDiskBalances(bottomProgram, 0)
