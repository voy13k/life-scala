object Life:

  def evolve(seed: Iterable[Position],
             maxCycles: Int,
             onCycle: (Long, Iterable[Position]) => Unit = (c, g) => ()
            ): Set[Position] =
    var liveCells = seed.toSet
    (1 to maxCycles).foreach(cycle =>
      liveCells = liveCells
        .flatMap(neighbouringPositions)
        .filter(_.shouldLive(liveCells))
      onCycle(cycle, liveCells)
    )
    liveCells
  end evolve

  /**
   * Calculate the life status in a new generation of a potential cell,
   * based on that cell's situation in previous generation,
   * according to the Game Rules:
   * 1. Any live cell with fewer than two live neighbors dies, as if by underpopulation.
   * 2. Any live cell with two or three live neighbors lives on to the next generation.
   * 3. Any live cell with more than three live neighbors dies, as if by overpopulation.
   * 4. Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.
   *
   * NOTE the rules above don't explicitly stipulate what happens to dead cells with other than 3 neighbours,
   * we will assume they implicitly stay dead.
   *
   * @param wasCellAlive           was the cell alive in the last generation
   * @param numberOfLiveNeighbours how many live neighbours has the cell had in the last generation
   * @return true when the cell is to live, false otherwise
   */
  def shouldLiveRules(wasCellAlive: Boolean, numberOfLiveNeighbours: Int): Boolean =
    numberOfLiveNeighbours match
      case x if wasCellAlive && x < 2 => false
      case 2 | 3 if wasCellAlive => true
      case x if wasCellAlive && x > 3 => false
      case 3 if !wasCellAlive => true
      case _ => false

  extension (p: Position)

    def neighbouringPositions: Seq[Position] = p match
      case Position(row, col) =>
        val ROW_ABOVE = row - 1
        val ROW_BELOW = row + 1
        val COL_LEFT = col - 1
        val COL_RIGHT = col + 1
        Seq(
          Position(ROW_ABOVE, COL_LEFT),
          Position(ROW_ABOVE, col),
          Position(ROW_ABOVE, COL_RIGHT),

          Position(row, COL_LEFT),
          Position(row, COL_RIGHT),

          Position(ROW_BELOW, COL_LEFT),
          Position(ROW_BELOW, col),
          Position(ROW_BELOW, COL_RIGHT),
        )

    private def shouldLive(generation: Set[Position]): Boolean = {
      val wasAlive = generation contains p
      val liveNeighbourCount = neighbouringPositions.count(generation.contains)
      shouldLiveRules(wasAlive, liveNeighbourCount)
    }
