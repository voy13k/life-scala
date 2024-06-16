case class Position(row: Long, col: Long):

  private def ABOVE_LEFT = Position(row - 1, col - 1)

  private def ABOVE = copy(row = row - 1)

  private def ABOVE_RIGHT = Position(row - 1, col + 1)

  private def LEFT = copy(col = col - 1)

  private def RIGHT = copy(col = col + 1)

  private def BELOW_LEFT = Position(row + 1, col - 1)

  private def BELOW = copy(row = row + 1)

  private def BELOW_RIGHT = Position(row + 1, col + 1)

  def neighbouringPositions: Vector[Position] = Vector(
      ABOVE_LEFT, ABOVE, ABOVE_RIGHT,
      LEFT, RIGHT,
      BELOW_LEFT, BELOW, BELOW_RIGHT
  )
