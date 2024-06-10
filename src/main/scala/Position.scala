case class Position(row: Long, col: Long):
  override def toString: String = String.format("[%d, %d]", row, col)
