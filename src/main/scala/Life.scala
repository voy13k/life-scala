@main def run(args: String*): Unit =
  val jsonString = if args.isEmpty then "[]" else args.mkString(" ")
  val life = parseJsonSeed(jsonString)
  life.evolve(100)

def parseJsonSeed(seedString: String): Life =
  val fromArgs: List[List[Int]] = upickle.default.read[List[List[Int]]](seedString)
  val seed = fromArgs.map(coords => Position(coords(0), coords(1)))
  Life(seed)

class Life(seed: Iterable[Position]):
  private var liveCells: Set[Position] = seed.toSet

  def evolve(maxCycles: Int): Set[Position] =
    for cycle <- 1 to maxCycles do
      liveCells = Life.evolve(liveCells)
      printf("%d: [%s]\n", cycle, liveCells.mkString(", "))

    liveCells

object Life:

  /**
   * Calculate the life status in a new generation of a potential cell,
   * based on that cell's situation in previous generation,
   * according to the Game Rules:
   * 1. Any live cell with fewer than two live neighbors dies, as if by underpopulation.
   * 2. Any live cell with two or three live neighbors lives on to the next generation.
   * 3. Any live cell with more than three live neighbors dies, as if by overpopulation.
   * 4. Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.
   *
   * @param wasCellAlive           was the cell alive in the last generation
   * @param numberOfLiveNeighbours how many live neighbours has the cell had in the last generation
   * @return true when the cell is to live, false otherwise
   */
  def shouldLiveRules(wasCellAlive: Boolean, numberOfLiveNeighbours: Int): Boolean =
    /*
     * The rules above can be logically transposed to:
     * A. Any cell with exactly 3 live neighbours (live or dead) is alive in next generation.
     * B. Any cell with 2 live neighbours retains their state in next generation (dead / alive).
     * C. In any other case a cell is dead in next generation.
     */
    numberOfLiveNeighbours match
      case 3 => true
      case 2 => wasCellAlive
      case _ => false

  private def evolve(oldGeneration: Set[Position]): Set[Position] =
    oldGeneration
      .flatMap(_.neighbouringPositions)
      .filter(_.shouldLive(oldGeneration))

  extension (p: Position)

    def neighbouringPositions: Seq[Position] =
      val ROW_ABOVE = p.row - 1
      val ROW_BELOW = p.row + 1
      val COL_LEFT = p.col - 1
      val COL_RIGHT = p.col + 1
      Seq(
        Position(ROW_ABOVE, COL_LEFT),
        Position(ROW_ABOVE, p.col),
        Position(ROW_ABOVE, COL_RIGHT),

        Position(p.row, COL_LEFT),
        Position(p.row, COL_RIGHT),

        Position(ROW_BELOW, COL_LEFT),
        Position(ROW_BELOW, p.col),
        Position(ROW_BELOW, COL_RIGHT),
      )

    private def shouldLive(otherLiveCells: Set[Position]): Boolean = {
      val wasAlive = otherLiveCells contains p
      val neighbourCount = p.liveNeighbourCount(otherLiveCells)
      shouldLiveRules(wasAlive, neighbourCount)
    }

    private def liveNeighbourCount(generation: Set[Position]): Int =
      p.neighbouringPositions.count(generation.contains)
