package voy13k.gameoflife

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import org.scalatest.prop.TableDrivenPropertyChecks

class GameOfLifeTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks with GivenWhenThen {

  "A new game" should "be blank" in {
    _givenNewGame._then.allCellsShouldBeDead
  }

  "A seeded game" should "include all seeds" in {
    _givenNewGame
      ._when.cellsSpawned((2, 4), (7, 1))
      ._then.onlyLiveCellsShouldBe((2, 4), (7, 1))
  }

  "A json seeded game" should "include all seeds" in {
    _givenNewGame
      ._when.cellsSpawned(
        """|[
           |  [1, 3],
           |  [1, 4],
           |  [3, 5]
           |]""".stripMargin)
      ._then.onlyLiveCellsShouldBe((1, 4), (3, 5), (1, 3))
  }

  "liveCellsJson" should "return valid json" in {
    val testJson = "[[1,3],[4,6],[-1,2]]"
    val fixture = _givenNewGame
      ._with.liveCells((1, 3), (4, 6), (-1, 2))
    fixture.game.liveCellsJson.replaceAll("\\s", "") should (
      equal("[[1,3],[4,6],[-1,2]]")
        or equal("[[1,3],[-1,2],[4,6]]")
        or equal("[[4,6],[1,3],[-1,2]]")
        or equal("[[4,6],[-1,2],[1,3]]")
        or equal("[[-1,2],[1,3],[4,6]]")
        or equal("[[-1,2],[4,6],[1,3]]")
      )
  }

  "Repeated seed" should "be alive" in {
    _givenNewGame
      ._when.cellsSpawned(
        """|[
           |  [1, 3],
           |  [1, 4],
           |  [1, 3]
           |]""".stripMargin)
      ._then.onlyLiveCellsShouldBe((1, 4), (1, 3))
  }

  "An invalid json jsonSeed" should "cause sensible exception" in {
    an[IllegalArgumentException] should be thrownBy {
      _givenNewGame._when.cellsSpawned("[[1]]")
    }
    Then("Exception should raised")
  }

  "Seeding" should "be cumulative" in {
    _givenNewGame
      ._when.cellsSpawned((1, 3)).cellsSpawned((2, 5))
      ._then.onlyLiveCellsShouldBe((1, 3), (2, 5))
  }

  "Killed cell" should "be dead" in {
    _givenNewGame
      ._with.liveCells((1, 4), (2, 3), (5, 9))
      ._when.cellsKilled((2, 3), (1, 4))
      ._then.onlyLiveCellsShouldBe((5, 9))
  }

  "Live cell with 0 neighbours" should "not survive single evolution" in {
    val cell = (1, 5)
    _givenNewGame
      ._with.liveCells(cell)
      ._when.evolved()
      ._then.shouldBeDead(cell)
  }

  "Dead cell with 0 neighbours" should "be dead after evolution" in {
    val cell = (1, 5)
    _givenNewGame
      ._with.deadCells(cell)
      ._when.evolved()
      ._then.shouldBeDead(cell)
  }

  "Live cell with 1 neighbour" should "not survive single evolution" in {
    val cell = (1, 5)
    forAll(neighbourCombinationsTable(cell, 1)) { neighbours =>
      _givenNewGame
        ._with.liveCellAndItsNeighbours(cell, neighbours)
        ._when.evolved()
        ._then.shouldBeDead(cell)
    }
  }

  "Dead cell with 1 neighbour" should "be dead after evolution" in {
    val cell = (1, 5)
    forAll(neighbourCombinationsTable(cell, 1)) { neighbours =>
      _givenNewGame
        ._with.deadCellAndItsNeighbours(cell, neighbours)
        ._when.evolved()
        ._then.shouldBeDead(cell)
    }
  }

  "Live cell with 2 neighbours" should "survive single evolution" in {
    val cell = (1, 5)
    forAll(neighbourCombinationsTable(cell, 2)) { neighbours =>
      _givenNewGame
        ._with.liveCellAndItsNeighbours(cell, neighbours)
        ._when.evolved()
        ._then.shouldBeAlive(cell)
    }
  }

  "Dead cell with 2 neighbours" should "be dead after single evolution" in {
    val cell = (1, 5)
    forAll(neighbourCombinationsTable(cell, 2)) { neighbours =>
      _givenNewGame
        ._with.deadCellAndItsNeighbours(cell, neighbours)
        ._when.evolved()
        ._then.shouldBeDead(cell)
    }
  }

  "Live cell with 3 neighbours" should "survive single evolution" in {
    val cell = (1, 5)
    forAll(neighbourCombinationsTable(cell, 3)) { neighbours =>
      _givenNewGame
        ._with.liveCellAndItsNeighbours(cell, neighbours)
        ._when.evolved()
        ._then.shouldBeAlive(cell)
    }
  }

  "Dead cell with 3 neighbours" should "be alive after single evolution" in {
    val cell = (1, 5)
    forAll(neighbourCombinationsTable(cell, 3)) { neighbours =>
      _givenNewGame
        ._with.deadCellAndItsNeighbours(cell, neighbours)
        ._when.evolved()
        ._then.shouldBeAlive(cell)
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
      forAll(neighbourCombinationsTable(cell, neighbourCount)) { neighbours =>
        _givenNewGame
          ._with.liveCellAndItsNeighbours(cell, neighbours)
          ._when.evolved()
          ._then.shouldBeDead(cell)
      }
    }
  }

  "A Penta-decathlon" should "have a period of 15" in {
    val pentaDecathlonSeed = Seq(
      (1, 1), (1, 2), (1, 3),
      (2, 1), /*   */ (2, 3),
      (3, 1), (3, 2), (3, 3),
      (4, 1), (4, 2), (4, 3),
      (5, 1), (5, 2), (5, 3),
      (6, 1), (6, 2), (6, 3),
      (7, 1), /*   */ (7, 3),
      (8, 1), (8, 2), (8, 3),
    )
    _givenNewGame
      ._with.liveCells(pentaDecathlonSeed: _*)
      ._then.evolutionShouldNotBePeriodicAtLeastUntil(15)
      ._thenWhen.evolved()
      ._then.onlyLiveCellsShouldBe(pentaDecathlonSeed: _*)
  }

  "A Diehard" should "disappear after 130" in {
    val diehardSeed = Seq(
      (1, 7),
      (2, 1), (2, 2),
      (3, 2), (3, 6), (3, 7), (3, 8),
    )
    _givenNewGame
      ._with.liveCells(diehardSeed: _*)
      ._then.evolutionShouldNotBePeriodicAtLeastUntil(130)
      ._thenWhen.evolved()
      ._then.allCellsShouldBeDead
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

  private def _givenNewGame: Fixture = {
    Given("a new game")
    Fixture()
  }

  private class Fixture:
    val game: GameOfLife = GameOfLife()

    var before: Set[(Int, Int)] = Set()

    var evolutionTrace: List[Set[(Int, Int)]] = List()

    def _with: Fixture = this

    def _when: Whens = Whens(this)

    def _then: Thens = Thens(this)

    def liveCells(cells: (Int, Int)*): Fixture =
      Given("live cells")
      game.spawn(cells)
      this

    def deadCells(cells: (Int, Int)*): Fixture =
      Given("dead cells")
      game.kill(cells)
      this

    def liveCellAndItsNeighbours(cell: (Int, Int), neighbours: Seq[(Int, Int)]): Fixture =
      Given(s"live cell and its ${neighbours.size} neighbours")
      game.spawn(Seq(cell))
      game.spawn(neighbours)
      this

    def deadCellAndItsNeighbours(cell: (Int, Int), neighbours: Seq[(Int, Int)]): Fixture =
      Given(s"dead cell and its ${neighbours.size} neighbours")
      game.kill(Seq(cell))
      game.spawn(neighbours)
      this

  private class Whens(val fixture: Fixture):
    def _then: Thens = Thens(fixture)

    def cellsSpawned(tuples: (Int, Int)*): Whens =
      When("cells spawned")
      fixture.game.spawn(tuples)
      this

    def cellsKilled(tuples: (Int, Int)*): Whens =
      When("cells killed")
      fixture.game.kill(tuples)
      this

    def cellsSpawned(json: String): Whens =
      When("cells spawned")
      fixture.game.spawn(json)
      this

    def evolved(): Whens =
      When("life evolves")
      fixture.game.evolve()
      this

  private class Thens(val fixture: Fixture):
    def _then: Thens = this

    def _thenWhen: Whens = fixture._when

    def onlyLiveCellsShouldBe(expectedLiveCells: (Int, Int)*): Thens =
      Then("only the specified cells should be alive")
      fixture.game.liveCells shouldBe expectedLiveCells.toSet
      this

    def allCellsShouldBeDead: Thens =
      Then("all cells should be dead")
      fixture.game.liveCells shouldBe empty
      this

    def shouldBeDead(cells: (Int, Int)*): Thens =
      Then("the specified cells should be dead")
      cells.foreach(c =>
        fixture.game.liveCells should not contain c
      )
      this

    def shouldBeAlive(cells: (Int, Int)*): Thens =
      Then("the specified cells should be alive")
      cells.foreach(c =>
        fixture.game.liveCells should contain(c)
      )
      this

    def evolutionShouldNotBePeriodicAtLeastUntil(period: Int): Thens = {
      fixture.evolutionTrace = List(fixture.game.liveCells)
      (1 until period).foreach { i =>
        fixture._when.evolved()

        Then(s"after evolution $i there should still be life")
        fixture.game.liveCells should not be empty

        Then("live cells should not match any prior state")
        fixture.evolutionTrace should not contain fixture.game.liveCells

        // add to trace for next round
        fixture.evolutionTrace = fixture.game.liveCells +: fixture.evolutionTrace
      }
      this
    }

}
