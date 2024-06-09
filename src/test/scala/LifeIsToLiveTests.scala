import TestUtils.fromMatrix
import org.scalatest.flatspec.AnyFlatSpec

class LifeIsToLiveTests extends AnyFlatSpec {

  private def anyLiveCellWithFewerThanTwoLiveNeighbours(wasAlive: Boolean, liveNeighbourCount: Int): Unit = {
    assert(wasAlive, "not applicable to dead cells")
    assert(liveNeighbourCount < 2, "not applicable to less than 2 neighbours")

    it must "die, as if by underpopulation" in {
      assertResult(false) {
        isToLive(wasAlive, liveNeighbourCount)
      }
    }
  }

  private def anyLiveCellWithTwoOrThreeLiveNeighbours(wasAlive: Boolean, liveNeighbourCount: Int): Unit = {
    assert(wasAlive, "not applicable to dead cells")
    assert(liveNeighbourCount == 2 || liveNeighbourCount == 3, "only applicable to 2 or 3 neighbours")

    it must "live on to the next generation" in {
      assertResult(true) {
        isToLive(wasAlive, liveNeighbourCount)
      }
    }
  }

  private def anyLiveCellWithMoreThanThreeLiveNeighbours(wasAlive: Boolean, liveNeighbourCount: Int): Unit = {
    assert(wasAlive, "not applicable to dead cells")
    assert(liveNeighbourCount > 3, "only applicable to more than 3 neighbours")

    it must "die, as if by overpopulation." in {
      assertResult(false) {
        isToLive(wasAlive, liveNeighbourCount)
      }
    }
  }

  private def anyDeadCellWithExactlyThreeLiveNeighbours(wasAlive: Boolean, liveNeighbourCount: Int): Unit = {
    assert(!wasAlive, "not applicable to live cells")
    assert(liveNeighbourCount == 3, "only applicable to 3 neighbours")

    it must "become a live cell, as if by reproduction" in {
      assertResult(true) {
        isToLive(wasAlive, liveNeighbourCount)
      }
    }
  }

  private def anyOtherCell(wasAlive: Boolean, liveNeighbourCount: Int): Unit = {
    assert(liveNeighbourCount != 3)
    assert(!wasAlive)

    it must "be dead in new generation" in {
      assertResult(false) {
        isToLive(wasAlive, liveNeighbourCount)
      }
    }
  }

  "0 neighbour live cell" must behave like anyLiveCellWithFewerThanTwoLiveNeighbours(true, 0)
  "1 neighbour live cell" must behave like anyLiveCellWithFewerThanTwoLiveNeighbours(true, 1)
  "2 neighbour live cell" must behave like anyLiveCellWithTwoOrThreeLiveNeighbours(true, 2)
  "3 neighbour live cell" must behave like anyLiveCellWithTwoOrThreeLiveNeighbours(true, 3)
  "4 neighbour live cell" must behave like anyLiveCellWithMoreThanThreeLiveNeighbours(true, 4)
  "5 neighbour live cell" must behave like anyLiveCellWithMoreThanThreeLiveNeighbours(true, 5)
  "6 neighbour live cell" must behave like anyLiveCellWithMoreThanThreeLiveNeighbours(true, 6)
  "7 neighbour live cell" must behave like anyLiveCellWithMoreThanThreeLiveNeighbours(true, 7)
  "8 neighbour live cell" must behave like anyLiveCellWithMoreThanThreeLiveNeighbours(true, 8)
  "0 neighbour dead cell" must behave like anyOtherCell(false, 0)
  "1 neighbour dead cell" must behave like anyOtherCell(false, 1)
  "2 neighbour dead cell" must behave like anyOtherCell(false, 2)
  "3 neighbour dead cell" must behave like anyDeadCellWithExactlyThreeLiveNeighbours(false, 3)
  "4 neighbour dead cell" must behave like anyOtherCell(false, 4)
  "5 neighbour dead cell" must behave like anyOtherCell(false, 5)
  "6 neighbour dead cell" must behave like anyOtherCell(false, 6)
  "7 neighbour dead cell" must behave like anyOtherCell(false, 7)
  "8 neighbour dead cell" must behave like anyOtherCell(false, 8)

}
