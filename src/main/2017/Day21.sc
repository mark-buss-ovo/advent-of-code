import scala.io.Source

val bufferedSource = Source.fromFile("/Users/mark.buss/Dev/algorithmic-code-club/advent-of-code/src/main/2017/Day21-input.txt")

case class Square(rows: Vector[Vector[Boolean]]) {
  def rotate: Square = {
    val newRows: Vector[Vector[Boolean]] = {
      rows.indices.map(index => {
        rows.indices.reverse.map(reverseIndex => {
          rows(reverseIndex)(index)
        }).toVector
      }).toVector
    }
    Square(newRows)
  }

  def flipHorizontal: Square = {
    val newRows: Vector[Vector[Boolean]] = {
      rows.map(_.reverse)
    }
    Square(newRows)
  }

  def flipVertical: Square = {
    val newRows: Vector[Vector[Boolean]] = rows.reverse
    Square(newRows)
  }

  def print: String = {
    rows.map(r => r.foldLeft("")((s, b) => b match {
      case true => s"$s#"
      case false => s"$s."
    })).mkString("\n")
  }

  def matches(other: Square): Boolean = {
    val allPermutations = Vector(this, rotate, rotate.rotate, rotate.rotate.rotate)
    val allCombinations = allPermutations.foldLeft(Vector.empty[Square])((allSquares, thisSquare) =>
    allSquares ++ Vector(thisSquare, thisSquare.flipHorizontal, thisSquare.flipVertical))

    allCombinations.contains(other)
  }

  def divide: Vector[Vector[Square]] = {
    val divider = rows.length % 2 match {
      case 0 => 2
      case _ => 3
    }

    val numberOfSquaresPerRow = rows.length / divider
    (0 until numberOfSquaresPerRow).map(rowIndex => {
      (0 until numberOfSquaresPerRow).map(colIndex => {
        val rowSet = rows.slice(rowIndex * divider, rowIndex * divider + divider)
        Square(rowSet.map(r =>
          r.slice(colIndex * divider, colIndex * divider + divider)
        ))
      }).toVector
    }).toVector
  }

  def countOn: Int = {
    rows.flatten.count(b => b)
  }
}

def fromString(input: Iterable[String]): Square = {
  Square(input.toVector.map(s => s.map {
    case '#' => true
    case '.' => false
  }.toVector))
}

def combine(divided: Vector[Vector[Square]]): Square = {
  val combinedRows = divided.indices.foldLeft(Vector.empty[Vector[Boolean]])((allRows, rowIndex) => {
    val rowOfSquares = divided(rowIndex)
    val squareSize = rowOfSquares.head.rows.length
    val rowsOfBooleans = (0 until squareSize).map(squareRowIndex =>
      rowOfSquares.foldLeft(Vector.empty[Boolean])((newRow, s) =>
        newRow ++ s.rows(squareRowIndex))).toVector
    allRows ++ rowsOfBooleans
  })
  Square(combinedRows)
}

val rules = bufferedSource.getLines.foldLeft(Map.empty[Square, Square])((map, line) => {
  if (line.isEmpty) {
    map
  }
  else {
    val parts = line.split(" => ")
    val originalSquare = fromString(parts(0).split('/'))
    val enhancedSquare = fromString(parts(1).split('/'))
    map.updated(originalSquare, enhancedSquare)
  }
})

val startingPattern = fromString(Vector(".#.", "..#", "###"))

def getEnhancement(square: Square): Square = {
  rules.find {
    case (original, _) => original.matches(square)
  }.get._2
}

def iterate(inputSquare: Square, totalIterations: Int, numberOfIterations: Int): Int = {
  val divided = inputSquare.divide
  val enhanced = divided.map(_.map(getEnhancement(_)))
  val combined = combine(enhanced)
  val newNumberOfIterations = numberOfIterations+1
  if(newNumberOfIterations == totalIterations) {
    combined.countOn
  } else {
    iterate(combined, totalIterations, newNumberOfIterations)
  }
}

iterate(startingPattern, 18, 0)