import org.scalatest.flatspec.AnyFlatSpec
import Life.neighbouringPositions

class PositionTests extends AnyFlatSpec {

  private val ROW = 0
  private val COL = 10
  private val neighbouringPositions = Position(ROW, COL).neighbouringPositions

  private val ROW_ABOVE = ROW - 1
  private val ROW_BELOW = ROW + 1
  private val COL_LEFT = COL - 1
  private val COL_RIGHT = COL + 1

  "Position neighbouringPositions" should "have size 8" in {
    assertResult(8) {
      neighbouringPositions.size
    }
  }

  it should "have 3 neighbours above" in {
    assert(neighbouringPositions contains Position(ROW_ABOVE, COL_LEFT))
    assert(neighbouringPositions contains Position(ROW_ABOVE, COL))
    assert(neighbouringPositions contains Position(ROW_ABOVE, COL_RIGHT))
  }

  it should "have left and right neighbours on the same row" in {
    assert(neighbouringPositions contains Position(ROW, COL_LEFT))
    assert(neighbouringPositions contains Position(ROW, COL_RIGHT))
  }

  it should "have 3 neighbours below" in {
    assert(neighbouringPositions contains Position(ROW_BELOW, COL_LEFT))
    assert(neighbouringPositions contains Position(ROW_BELOW, COL))
    assert(neighbouringPositions contains Position(ROW_BELOW, COL_RIGHT))
  }

}
