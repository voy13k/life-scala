@main def run(args: String*): Unit =
  val seed = if args.isEmpty then "[]" else args.mkString(" ")
  val fromArgs: List[List[Int]] = upickle.default.read[List[List[Int]]](seed)
  var generation = fromArgs
    .map(coords => Position(coords(0), coords(1)))
    .toSet
  for i <- 1 to 100 do
    generation = tick(generation)
    printf("%d: [%s]\n", i, generation.mkString(", "))

def tick(oldGeneration: Set[Position]): Set[Position] =
  oldGeneration
    .flatMap(p => p +: p.neighbouringPositions)
    .filter(p => isToLive(p, oldGeneration))

private def isToLive(p: Position, oldGeneration: Set[Position]): Boolean = {
  val neighbourCount = liveNeighbourCount(p, oldGeneration)
  val wasAlive = oldGeneration contains p
  isToLive(wasAlive, neighbourCount)
}

private def liveNeighbourCount(position: Position, generation: Set[Position]) =
  position.neighbouringPositions.count(generation.contains)

private def isToLive(wasItselfAlive: Boolean, numberOfLiveNeighbours: Int): Boolean =
  /*
   * Game rules
   * 1. Any live cell with fewer than two live neighbors dies, as if by underpopulation.
   * 2. Any live cell with two or three live neighbors lives on to the next generation.
   * 3. Any live cell with more than three live neighbors dies, as if by overpopulation.
   * 4. Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.
   *
   * These rules can be logically transposed to:
   * A. Any cell with exactly three live neighbours (live or dead) is alive in next generation.
   * B. Any cell with two live neighbours retains their state in next generation.
   * C. All other cells are dead in next generation.
   */
  numberOfLiveNeighbours match
    case 3 => true
    case 2 => wasItselfAlive
    case _ => false
