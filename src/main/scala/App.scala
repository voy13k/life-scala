import voy13k.gameoflife.GameOfLife

@main def app(args: String*): Unit =
  val jsonString = if args.isEmpty then "[]" else args.mkString(" ")
  val game = new GameOfLife
  game.spawn(jsonString)
  (1 to 100).foreach { cycle =>
    game.evolve()
    println(s"$cycle: ${game.liveCellsJson}")
  }
