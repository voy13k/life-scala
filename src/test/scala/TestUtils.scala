import scala.collection.mutable

object TestUtils {

  def fromMatrix(matrix: String): Set[Position] =
    var positions = Set[Position]()
    val lines = matrix.lines.toList
    for row <- 0 until lines.size do
      val line = lines.get(row)
      for
        col <- 0 until line.length
        if line.charAt(col) != ' '
      do
        positions = positions + Position(row, col)

    positions

}
