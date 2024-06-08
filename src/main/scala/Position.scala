case class Position(row: Long, col: Long) {

  override def toString: String = String.format("[%d, %d]", row, col)

  private val ROW_ABOVE = row - 1
  private val ROW_BELOW = row + 1
  private val COL_LEFT = col - 1
  private val COL_RIGHT = col + 1

  def neighboursOf: Seq[Position] = Seq(
    Position(ROW_ABOVE, COL_LEFT),
    Position(ROW_ABOVE, col),
    Position(ROW_ABOVE, COL_RIGHT),

    Position(row, COL_LEFT),
    Position(row, COL_RIGHT),

    Position(ROW_BELOW, COL_LEFT),
    Position(ROW_BELOW, col),
    Position(ROW_BELOW, COL_RIGHT),
  )

}
