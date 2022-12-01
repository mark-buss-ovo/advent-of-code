import scala.io.Source

val bufferedSource = Source.fromFile("/Users/mark.buss/Dev/algorithmic-code-club/advent-of-code/src/main/2017/Day24-input.txt")

case class Port(id: Int, numberOfPins: Int, used: Boolean)

case class Component(id: Int, ports: Vector[Port]) {
  def canLink(numberOfPins: Int): Boolean =
    ports.exists(p => !p.used && p.numberOfPins == numberOfPins)

  def link(numberOfPins: Int): Component = {
    val linkedPort = ports.filter(p => !p.used && p.numberOfPins == numberOfPins).head.copy(used = true)
    val otherPorts = ports.filterNot(_.id == linkedPort.id)
    copy(ports = linkedPort +: otherPorts)
  }

  def availablePortNumberOfPins: Int = {
    ports.filterNot(_.used).head.numberOfPins
  }

  def strength: Int = {
    ports.foldLeft(0)((sum, port) => sum + port.numberOfPins)
  }
}

case class Bridge(components: Vector[Component]) {
  def endOfBridgeNumberOfPins: Int = {
    if (components.isEmpty) 0 else components.last.availablePortNumberOfPins
  }

  def link(component: Component): Bridge = {
    val linked = component.link(endOfBridgeNumberOfPins)
    copy(components :+ linked)
  }

  def strength: Int =
    components.foldLeft(0)((sum, component) => sum + component.strength)

  def print: Unit = {
    println(components.map(c => c.ports.map(p => p.numberOfPins).mkString("/")).mkString(" -> "))
  }

  def length: Int =
    components.length
}

val components = bufferedSource.getLines.foldLeft(Vector.empty[Component])((componentList, line) => {
  if (line.isEmpty) {
    componentList
  }
  else {
    println(line)
    val portStrings = line.split('/')
    val ports = portStrings.indices.map(i => Port(i, portStrings(i).toInt, false)).toVector
    componentList.appended(Component(componentList.length, ports))
  }
})

println(components)

def buildBridges(startBridge: Bridge, availableComponents: Vector[Component], bridgesSoFar: Vector[Bridge]): Vector[Bridge] = {
  val pinsToMatch = startBridge.endOfBridgeNumberOfPins
  val matchingComponents = availableComponents.filter(_.canLink(pinsToMatch))
  if (matchingComponents.isEmpty) {
    bridgesSoFar :+ startBridge
  } else {
    bridgesSoFar ++ matchingComponents.flatMap(c => {
      val newBridge = startBridge.link(c)
      val newComponents = availableComponents.filterNot(a => a.id == c.id)
      buildBridges(newBridge, newComponents, bridgesSoFar)
    })
  }
}

val allBridges = buildBridges(Bridge(Vector.empty[Component]), components, Vector.empty[Bridge])

allBridges.map(_.strength).sorted(Ordering[Int].reverse).head

val longest = allBridges.map(_.length).sorted(Ordering[Int].reverse).head

val longestBridges = allBridges.filter(_.length == longest)

longestBridges.map(_.strength).sorted(Ordering[Int].reverse).head

