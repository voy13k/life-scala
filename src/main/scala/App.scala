@main def app(args: String*): Unit =
  val jsonString = if args.isEmpty then "[]" else args.mkString(" ")
  val seed = parseJsonSeed(jsonString)
  val lifePrinter = LifePrinter()
  Life(seed).take(100).foreach(lifePrinter.printLife)

private def parseJsonSeed(seedString: String): List[Position] =
  val fromArgs = upickle.default.read[List[Vector[Int]]](seedString)
  fromArgs.map(v => Position(v(0), v(1)))

class LifePrinter:
  var cycle = 1

  def printLife(liveCells: Iterable[Position]): Unit = {
    printf(
      "%d: [%s]\n",
      cycle,
      liveCells.map(c => s"[${c.row}, ${c.col}]").mkString(", "))
    cycle = cycle + 1
  }
