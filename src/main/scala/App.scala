@main def app(args: String*): Unit =
  val jsonString = if args.isEmpty then "[]" else args.mkString(" ")
  val seed = parseJsonSeed(jsonString)
  printLife(0, seed)
  Life.evolve(seed, 100, printLife)

private def parseJsonSeed(seedString: String): List[Position] =
  val fromArgs = upickle.default.read[List[Vector[Int]]](seedString)
  fromArgs.map(v => Position(v(0), v(1)))

private def printLife(cycle: Long, liveCells: Iterable[Position]): Unit = {
  printf(
    "%d: [%s]\n",
    cycle,
    liveCells.map(c => s"[${c.row}, ${c.col}]").mkString(", "))
}
