val buffer = Vector(0)
val stepCount = 367
val maxIterations = 2017;



def doTheThing(iteration: Int, currentIndex: Int, buffer: Vector[Int]): Int = {
  val increasedIndex = ((currentIndex + stepCount)%iteration)+1

  val (front, back) = buffer.splitAt(increasedIndex)
  val newBuffer = front ++ List(iteration) ++ back

  println(newBuffer)

  if(iteration == maxIterations){
    newBuffer(increasedIndex + 1)
  } else{
    doTheThing(iteration+1, increasedIndex, newBuffer)
  }
}

def part2(iteration: Int, currentIndex: Int, secondValue: Int): Int = {
  val increasedIndex = ((currentIndex + stepCount)%iteration)+1

  val updatedSecondValue = increasedIndex match {
    case 1 => iteration
    case _ => secondValue
  }

  if(iteration == 50000000){
    updatedSecondValue
  } else {
    part2(iteration+1, increasedIndex, updatedSecondValue)
  }
}


part2(1, 0, -1)