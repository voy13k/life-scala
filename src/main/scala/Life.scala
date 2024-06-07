@main def run(args: String*): Unit =
  val seed = if args.isEmpty then "[]" else args.mkString(" ")
  val fromArgs: List[List[Int]] = upickle.default.read[List[List[Int]]](seed)
  var generation = fromArgs
    .map(coords => Position(coords(0), coords(1)))
    .toSet
  for i <- 1 to 100 do
    generation = tick(generation)
    printf("%d: %s\n", i, toString(generation))

private def toString(positions: Iterable[Position]): String =
  "[" + positions.mkString(", ") + "]"

def tick(oldGeneration: Set[Position]): Set[Position] =
  oldGeneration
    .flatMap(p => p +: neighboursOf(p))
    .filter(p => liveNeighbourCount(p, oldGeneration) match
      case 2 => oldGeneration contains p
      case 3 => true
      case _ => false
    )

private def neighboursOf(position: Position): Seq[Position] = List(
  Position(position.row - 1, position.col - 1),
  Position(position.row - 1, position.col),
  Position(position.row - 1, position.col + 1),
  Position(position.row, position.col - 1),
  Position(position.row, position.col + 1),
  Position(position.row + 1, position.col - 1),
  Position(position.row + 1, position.col),
  Position(position.row + 1, position.col + 1),
)

private def liveNeighbourCount(position: Position, generation: Set[Position]) =
  neighboursOf(position)
    .count(generation.contains)
