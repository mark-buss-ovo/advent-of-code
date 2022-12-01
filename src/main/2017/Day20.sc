  import scala.io.Source
  import scala.util.matching.Regex

  val bufferedSource = Source.fromFile("/Users/mark.buss/Dev/algorithmic-code-club/advent-of-code/src/main/2017/Day20-input.txt")

  case class Coordinates(x: Int, y: Int, z: Int)

  case class Particle(id: Int, position: Coordinates, velocity: Coordinates, acceleration: Coordinates, distance: Int, movingAway: Option[Boolean])

  val particleRegex: Regex = "p=<(-?\\d+),(-?\\d+),(-?\\d+)>, v=<(-?\\d+),(-?\\d+),(-?\\d+)>, a=<(-?\\d+),(-?\\d+),(-?\\d+)>".r
  val startingParticles = bufferedSource.getLines.foldLeft(Vector.empty[Particle])((list, line) => {
    if (line.isEmpty) {
      list
    }
    else {
      val particleParts = particleRegex.findAllMatchIn(line)
      val matches = particleParts.toList.head
      val pX = matches.group(1).toInt
      val pY = matches.group(2).toInt
      val pZ = matches.group(3).toInt
      val vX = matches.group(4).toInt
      val vY = matches.group(5).toInt
      val vZ = matches.group(6).toInt
      val aX = matches.group(7).toInt
      val aY = matches.group(8).toInt
      val aZ = matches.group(9).toInt

      list.appended(Particle(list.size, Coordinates(pX, pY, pZ), Coordinates(vX, vY, vZ), Coordinates(aX, aY, aZ), pX.abs + pY.abs + pZ.abs, None))
    }
  })

def tick(particles: Vector[Particle],
         closest: Particle,
         iterations: Int): Int = {
  val newParticles = particles.map(p => {
    val newVelocity = p.velocity.copy(
      p.velocity.x + p.acceleration.x,
      p.velocity.y + p.acceleration.y,
      p.velocity.z + p.acceleration.z
    )

    val newPosition = p.position.copy(
      p.position.x + newVelocity.x,
      p.position.y + newVelocity.y,
      p.position.z + newVelocity.z
    )

    val newDistance = newPosition.x.abs + newPosition.y.abs + newPosition.z.abs
    val movingAway = newDistance > p.distance
    p.copy(position = newPosition, velocity = newVelocity, distance = newDistance, movingAway = Some(movingAway))
  })

  val newSorted = newParticles.sortBy(p => p.distance)
  val newClosest = newSorted.head

  if(iterations > 1000){
    newClosest.id
  } else {
    println(s"Iteration: $iterations, Closest: ${closest.id}")
    tick(newSorted, newClosest, iterations+1)
  }

}

val sortedParticles = startingParticles.sortBy(p => p.distance)
val closest = sortedParticles.head
val secondClosest = sortedParticles(1)

//tick(sortedParticles, closest, 0)


def tickPartTwo(particles: Vector[Particle],
         iterations: Int): Int = {
  val newParticles = particles.map(p => {
    val newVelocity = p.velocity.copy(
      p.velocity.x + p.acceleration.x,
      p.velocity.y + p.acceleration.y,
      p.velocity.z + p.acceleration.z
    )

    val newPosition = p.position.copy(
      p.position.x + newVelocity.x,
      p.position.y + newVelocity.y,
      p.position.z + newVelocity.z
    )

    val newDistance = newPosition.x.abs + newPosition.y.abs + newPosition.z.abs
    val movingAway = newDistance > p.distance
    p.copy(position = newPosition, velocity = newVelocity, distance = newDistance, movingAway = Some(movingAway))
  })

  val collisions = newParticles.groupBy(p => p.position).filter {
    case (_, members) => members.length > 1
  }.keys.toVector

  val particlesNotCollided = newParticles.filterNot(p => collisions.contains(p.position))

  if(iterations > 1000) {
    particlesNotCollided.length
  } else {
    println(s"Iteration: $iterations, Remaining Particles: ${particlesNotCollided.length}")
    tickPartTwo(particlesNotCollided, iterations+1)
  }
}

tickPartTwo(startingParticles, 0)