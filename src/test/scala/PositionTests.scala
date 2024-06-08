import org.scalatest.flatspec.AnyFlatSpec

class PositionTests extends AnyFlatSpec {

  private val ROW = 0
  private val COL = 10
  private val neighbours = Position(ROW, COL).neighboursOf

  private val ROW_ABOVE = ROW - 1
  private val ROW_BELOW = ROW + 1
  private val COL_LEFT = COL - 1
  private val COL_RIGHT = COL + 1

  "Position neighboursOf" should "have size 8" in {
    assertResult(8) {
      neighbours.size
    }
  }

  it should "have 3 neighbours above" in {
    assert(neighbours contains Position(ROW_ABOVE, COL_LEFT))
    assert(neighbours contains Position(ROW_ABOVE, COL))
    assert(neighbours contains Position(ROW_ABOVE, COL_RIGHT))
  }

  it should "have left and right neighbours on the same row" in {
    assert(neighbours contains Position(ROW, COL_LEFT))
    assert(neighbours contains Position(ROW, COL_RIGHT))
  }

  it should "have 3 neighbours below" in {
    assert(neighbours contains Position(ROW_BELOW, COL_LEFT))
    assert(neighbours contains Position(ROW_BELOW, COL))
    assert(neighbours contains Position(ROW_BELOW, COL_RIGHT))
  }

}
