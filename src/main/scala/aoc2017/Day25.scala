package aoc

object Day25 {

  val checksumSteps = 6
  private val tape = Map.empty[Int, Boolean].withDefault(_ => false)
  private val sampleBluePrint = Blueprint(
    Map(
      'A' -> State(
        Map(false -> Rule(true, false, 'B'), true -> Rule(false, true, 'B'))
      ),
      'B' -> State(
        Map(
          false -> Rule(true, true, 'A'),
          true -> Rule(true, false, 'A')
        )
      )
    )
  )

  private val actualBluePrint = Blueprint(
    Map(
      'A' -> State(
        Map(
          false -> Rule(true, false, 'B'),
          true -> Rule(false, true, 'F')
        )
      ),
      'B' -> State(
        Map(
          false -> Rule(false, false, 'C'),
          true -> Rule(false, false, 'D')
        )
      ),
      'C' -> State(
        Map(
          false -> Rule(true, true, 'D'),
          true -> Rule(true, false, 'E')
        )
      ),
      'D' -> State(
        Map(
          false -> Rule(false, true, 'E'),
          true -> Rule(false, true, 'D')
        )
      ),
      'E' -> State(
        Map(
          false -> Rule(false, false, 'A'),
          true -> Rule(true, false, 'C')
        )
      ),
      'F' -> State(
        Map(
          false -> Rule(true, true, 'A'),
          true -> Rule(true, false, 'A')
        )
      )
    )
  )

  def main(args: Array[String]): Unit = {
    //sample
    // println(step(sampleBluePrint, tape, 0, 'A', 0, 6))

    //part 1
    println(step(actualBluePrint, tape, 0, 'A', 0, 12794428))
  }

  def step(
      blueprint: Blueprint,
      tape: Map[Int, Boolean],
      cursor: Int,
      currentState: Char,
      stepsSoFar: Int,
      totalSteps: Int
  ): Int = {
    if (stepsSoFar == totalSteps) {
      tape.values.count(v => v)
    } else {
      val slotValue = tape(cursor)
      val rule = blueprint.states(currentState).rules(slotValue)

      val nextCursor = if (rule.left) cursor - 1 else cursor + 1
      step(
        blueprint,
        tape.updated(cursor, rule.write),
        nextCursor,
        rule.nextState,
        stepsSoFar + 1,
        totalSteps
      )
    }
  }

  case class Rule(write: Boolean, left: Boolean, nextState: Char)

  case class State(rules: Map[Boolean, Rule])

  case class Blueprint(states: Map[Char, State])
}
