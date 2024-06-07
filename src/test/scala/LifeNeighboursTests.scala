import TestUtils.fromMatrix
import org.scalatest.flatspec.AnyFlatSpec

class LifeNeighboursTests extends AnyFlatSpec {

  private val ROW = 0
  private val COL = 10
  private val ABOVE = ROW - 1
  private val BELOW = ROW + 1
  private val LEFT = COL - 1
  private val RIGHT = COL + 1
  private val neighbours = neighboursOf(Position(ROW, COL))

  "neighboursOf Position" should "have size 8" in {
    assertResult(8) {
      neighbours.size
    }
  }

  it should "have 3 neighbours above" in {
    assert(neighbours contains Position(ABOVE, LEFT))
    assert(neighbours contains Position(ABOVE, COL))
    assert(neighbours contains Position(ABOVE, RIGHT))
  }

  it should "have left and right neighbours on the same row" in {
    assert(neighbours contains Position(ROW, LEFT))
    assert(neighbours contains Position(ROW, RIGHT))
  }

  it should "have 3 neighbours below" in {
    assert(neighbours contains Position(BELOW, LEFT))
    assert(neighbours contains Position(BELOW, COL))
    assert(neighbours contains Position(BELOW, RIGHT))
  }

}
