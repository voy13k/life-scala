package voy13k.gameoflife

import upickle.core.AbortException


class GameOfLife:

  private var cells: Set[(Int, Int)] = Set()

  def liveCells: Set[(Int, Int)] = cells

  def spawn(seed: Iterable[(Int, Int)]): Unit =
    cells = cells ++ seed

  def spawn(jsonSeed: String): Unit =
    spawn(GameOfLife.parse(jsonSeed))

  def kill(seed: Iterable[(Int, Int)]): Unit =
    cells = cells -- seed

  def evolve(cycleCount: Int = 1): Unit =
    cells = cells
      .flatMap(GameOfLife.neighbourhoodOf)
      .filter(GameOfLife.shouldLive(_, cells))


private object GameOfLife:
  private def neighbourhoodOf(cell: (Int, Int)) =
    val (row, col) = cell

    def rowAbove = row - 1

    def rowBelow = row + 1

    def colLeft = col - 1

    def colRight = col + 1

    Seq(
      (rowAbove, colLeft),
      (rowAbove, col),
      (rowAbove, colRight),
      (row, colLeft),
      (row, colRight),
      (rowBelow, colLeft),
      (rowBelow, col),
      (rowBelow, colRight),
    )

  private def shouldLive(cell: (Int, Int), prevGeneration: Set[(Int, Int)]): Boolean =
    val isCellAlive = prevGeneration.contains(cell)
    val liveNeighbourCount = neighbourhoodOf(cell).count(prevGeneration.contains)

    /*
     * Conway's Game Of Life rules
     */
    (isCellAlive, liveNeighbourCount) match
      // Rule 1. Any live cell with fewer than two live neighbours dies, as if by underpopulation.
      case (true, c) if c < 2 => false

      // Rule 2. Any live cell with two or three live neighbours lives on to the next generation.
      case (true, c) if c == 2 || c == 3 => true

      // Rule 3. Any live cell with more than three live neighbours dies, as if by overpopulation.
      case (true, c) if c > 3 => false

      // Rule 4. Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
      case (false, 3) => true

      // Implicit catch all - all other cells die
      case _ => false

  private def parse(jsonSeed: String) = {
    try
      upickle.default.read[Seq[(Int, Int)]](jsonSeed)
    catch
      case e: AbortException => throw IllegalArgumentException("JSON in the form of [[x1,y1],[x2,y2], ...] expected", e)
  }

