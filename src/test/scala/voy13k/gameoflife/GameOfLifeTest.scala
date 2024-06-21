package voy13k.gameoflife

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import org.scalatest.prop.TableDrivenPropertyChecks

class GameOfLifeTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  "A new game" should "be blank" in new GameContext {
    _then.allCellsShouldBeDead
  }

  "A seeded game" should "include all seeds" in new GameContext {
    _when.cellsSpawned((2, 4), (7, 1))
    _then.onlyLiveCellsShouldBe((2, 4), (7, 1))
  }

  "A json seeded game" should "include all seeds" in new GameContext {
    _when.cellsSpawned(
      """|[
         |  [1, 3],
         |  [1, 4],
         |  [3, 5]
         |]""".stripMargin)
    _then.onlyLiveCellsShouldBe((1, 4), (3, 5), (1, 3))
  }

  "liveCellsJson" should "return valid json" in new GameContext {
    _with.liveCells((1, 3), (4, 6), (-1, 2))
    game.liveCellsJson.replaceAll("\\s", "") should (
      equal("[[1,3],[4,6],[-1,2]]")
        or equal("[[1,3],[-1,2],[4,6]]")
        or equal("[[4,6],[1,3],[-1,2]]")
        or equal("[[4,6],[-1,2],[1,3]]")
        or equal("[[-1,2],[1,3],[4,6]]")
        or equal("[[-1,2],[4,6],[1,3]]")
      )
  }

  "Repeated seed" should "be alive" in new GameContext {
    _when.cellsSpawned(
      """|[
         |  [1, 3],
         |  [1, 4],
         |  [1, 3]
         |]""".stripMargin)
    _then.onlyLiveCellsShouldBe((1, 4), (1, 3))
  }

  "An invalid json seed" should "cause sensible exception" in new GameContext {
    an[IllegalArgumentException] should be thrownBy {
      _when.cellsSpawned("[[1]]")
    }
  }

  "Seeding" should "be cumulative" in new GameContext {
    _when.cellsSpawned((1, 3)).cellsSpawned((2, 5))
    _then.onlyLiveCellsShouldBe((1, 3), (2, 5))
  }

  "Killed cell" should "be dead" in new GameContext {
    _with.liveCells((1, 4), (2, 3), (5, 9))
    _when.cellsKilled((2, 3), (1, 4))
    _then.onlyLiveCellsShouldBe((5, 9))
  }

  "Live cell with 0 neighbours" should "not survive single evolution" in new GameContext {
    private val cell = (1, 5)
    _with.liveCells(cell)
    _when.evolved()
    _then.shouldBeDead(cell)
  }

  "Dead cell with 0 neighbours" should "be dead after evolution" in new GameContext {
    private val cell = (1, 5)
    _with.deadCells(cell)
    _when.evolved()
    _then.shouldBeDead(cell)
  }

  "Live cell with 1 neighbour" should "not survive single evolution" in {
    val cell = (1, 5)
    forAll(neighbourCombinationsTable(cell, 1)) { neighbours =>
      new GameContext {
        _with.liveCellAndItsNeighbours(cell, neighbours)
        _when.evolved()
        _then.shouldBeDead(cell)
      }
    }
  }

  "Dead cell with 1 neighbour" should "be dead after evolution" in {
    val cell = (1, 5)
    forAll(neighbourCombinationsTable(cell, 1)) { neighbours =>
      new GameContext {
        _with.deadCellAndItsNeighbours(cell, neighbours)
        _when.evolved()
        _then.shouldBeDead(cell)
      }
    }
  }

  "Live cell with 2 neighbours" should "survive single evolution" in {
    val cell = (1, 5)
    forAll(neighbourCombinationsTable(cell, 2)) { neighbours =>
      new GameContext {
        _with.liveCellAndItsNeighbours(cell, neighbours)
        _when.evolved()
        _then.shouldBeAlive(cell)
      }
    }
  }

  "Dead cell with 2 neighbours" should "be dead after single evolution" in {
    val cell = (1, 5)
    forAll(neighbourCombinationsTable(cell, 2)) { neighbours =>
      new GameContext {
        _with.deadCellAndItsNeighbours(cell, neighbours)
        _when.evolved()
        _then.shouldBeDead(cell)
      }
    }
  }

  "Live cell with 3 neighbours" should "survive single evolution" in {
    val cell = (1, 5)
    forAll(neighbourCombinationsTable(cell, 3)) { neighbours =>
      new GameContext {
        _with.liveCellAndItsNeighbours(cell, neighbours)
        _when.evolved()
        _then.shouldBeAlive(cell)
      }
    }
  }

  "Dead cell with 3 neighbours" should "be alive after single evolution" in {
    val cell = (1, 5)
    forAll(neighbourCombinationsTable(cell, 3)) { neighbours =>
      new GameContext {
        _with.deadCellAndItsNeighbours(cell, neighbours)
        _when.evolved()
        _then.shouldBeAlive(cell)
      }
    }
  }

  "Live cell with more than 3 neighbours" should "be dead after single evolution" in {
    val cell = (1, 5)
    forAll(
      Table(
        "neighbourCount",
        4 to 8: _*
      )
    ) { neighbourCount =>
      new GameContext {
        forAll(neighbourCombinationsTable(cell, neighbourCount)) { neighbours =>
          _with.liveCellAndItsNeighbours(cell, neighbours)
          _when.evolved()
          _then.shouldBeDead(cell)
        }
      }
    }
  }

  "A Penta-decathlon" should "have a period of 15" in new GameContext {
    private val pentaDecathlonSeed = Seq(
      (1, 1), (1, 2), (1, 3),
      (2, 1), /*   */ (2, 3),
      (3, 1), (3, 2), (3, 3),
      (4, 1), (4, 2), (4, 3),
      (5, 1), (5, 2), (5, 3),
      (6, 1), (6, 2), (6, 3),
      (7, 1), /*   */ (7, 3),
      (8, 1), (8, 2), (8, 3),
    )
    _with.liveCells(pentaDecathlonSeed: _*)
    _then.evolutionShouldNotBePeriodicAtLeastUntil(15)
    _thenWhen.evolved()
    _then.onlyLiveCellsShouldBe(pentaDecathlonSeed: _*)
  }

  "A Diehard" should "disappear after 130" in new GameContext {
    private val diehardSeed = Seq(
      (1, 7),
      (2, 1), (2, 2),
      (3, 2), (3, 6), (3, 7), (3, 8),
    )
    _with.liveCells(diehardSeed: _*)
    _then.evolutionShouldNotBePeriodicAtLeastUntil(130)
    _thenWhen.evolved()
    _then.allCellsShouldBeDead
  }

  private def neighbourCombinationsTable(cell: (Int, Int), neighbourCount: Int) = {
    val allNeighbours = cell match
      case (row, col) => Set(
        (row - 1, col - 1),
        (row - 1, col),
        (row - 1, col + 1),
        (row, col - 1),
        (row, col + 1),
        (row + 1, col - 1),
        (row + 1, col),
        (row + 1, col - 1)
      )

    val allCombinations = Combinations
      .allNCombinations(neighbourCount, allNeighbours)
      .map(set => set.toSeq)
      .toSeq

    Table(
      "neighbours",
      allCombinations: _*
    )

  }

  class GameContext:
    val _with: Builder = Builder()
    val _when: Whens = Whens()
    val _then: Thens = Thens()
    val _thenWhen: Whens = _when

    val game: GameOfLife = GameOfLife()

    private var evolutionTrace: List[Set[(Int, Int)]] = List()

    class Builder:
      def liveCells(cells: (Int, Int)*): Builder =
        game.spawn(cells)
        this

      def deadCells(cells: (Int, Int)*): Builder =
        game.kill(cells)
        this

      def liveCellAndItsNeighbours(cell: (Int, Int), neighbours: Seq[(Int, Int)]): Builder =
        game.spawn(Seq(cell))
        game.spawn(neighbours)
        this

      def deadCellAndItsNeighbours(cell: (Int, Int), neighbours: Seq[(Int, Int)]): Builder =
        game.kill(Seq(cell))
        game.spawn(neighbours)
        this

    class Whens:
      def cellsSpawned(tuples: (Int, Int)*): Whens =
        game.spawn(tuples)
        this

      def cellsKilled(tuples: (Int, Int)*): Whens =
        game.kill(tuples)
        this

      def cellsSpawned(json: String): Whens =
        game.spawn(json)
        this

      def evolved(): Whens =
        game.evolve()
        evolutionTrace = game.liveCells +: evolutionTrace
        this

    class Thens:
      def onlyLiveCellsShouldBe(expectedLiveCells: (Int, Int)*): Thens =
        game.liveCells shouldBe expectedLiveCells.toSet
        this

      def allCellsShouldBeDead: Thens =
        game.liveCells shouldBe empty
        this

      def shouldBeDead(cells: (Int, Int)*): Thens =
        cells.foreach(c =>
          game.liveCells should not contain c
        )
        this

      def shouldBeAlive(cells: (Int, Int)*): Thens =
        cells.foreach(c =>
          game.liveCells should contain(c)
        )
        this

      private def shouldBeAlive: Thens =
        game.liveCells should not be empty
        this

      private def shouldBeUnique: Thens =
        evolutionTrace.tail should not contain game.liveCells
        this

      def evolutionShouldNotBePeriodicAtLeastUntil(period: Int): Thens =
        evolutionTrace = List(game.liveCells)
        (1 until period).foreach { i =>
          _when.evolved()
          _then
            .shouldBeAlive
            .shouldBeUnique
        }
        this

}
