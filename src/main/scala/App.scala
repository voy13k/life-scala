@main def app(args: String*): Unit =
  val jsonString = if args.isEmpty then "[]" else args.mkString(" ")
  val life = parseJsonSeed(jsonString)
  life.evolve(100, (c, g) => printLife(c, g))

private def parseJsonSeed(seedString: String) =
  val fromArgs = upickle.default.read[List[Vector[Int]]](seedString)
  val seed = fromArgs.map(v => Position(v(0), v(1)))
  printLife(0, seed)
  Life(seed)

private def printLife(c: Long, g: Iterable[Position]): Unit = {
  printf(
    "%d: [%s]\n",
    c,
    g.map(c => s"[${c.row}, ${c.col}]").mkString(", "))
}
