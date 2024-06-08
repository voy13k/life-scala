case class Position(row: Long, col: Long) {

  override def toString: String = String.format("[%d, %d]", row, col)

  def neighboursOf: Seq[Position] = Seq(
    Position(row - 1, col - 1),
    Position(row - 1, col),
    Position(row - 1, col + 1),
    Position(row, col - 1),
    Position(row, col + 1),
    Position(row + 1, col - 1),
    Position(row + 1, col),
    Position(row + 1, col + 1),
  )

}
