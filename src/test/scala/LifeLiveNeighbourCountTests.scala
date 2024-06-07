import TestUtils.fromMatrix
import org.scalatest.flatspec.AnyFlatSpec

class LifeLiveNeighbourCountTests extends AnyFlatSpec {

  private def assertLiveNeighbourCountInMatrix(position: Position, expectedCount: Int, matrix: String): Any =
    assertResult(expectedCount) {
      liveNeighbourCount(position, fromMatrix(matrix.stripMargin))
    }

  "liveNeighbourCount of Position(1,1)" should "be 0" in {
    assertLiveNeighbourCountInMatrix(Position(1, 1), 0,
      """
        | X
        |   """)
  }

  it should "be 1" in {
    assertLiveNeighbourCountInMatrix(Position(1, 1), 1,
      """X
        | X
        |   """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 1,
      """ X
        | X
        |   """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 1,
      """  X
        | X
        |   """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 1,
      """
        |XX
        |   """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 1,
      """
        | XX
        |   """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 1,
      """
        | X
        |X  """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 1,
      """
        | X
        | X """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 1,
      """
        | X
        |  X""")
  }

  it should "be 2" in {
    assertLiveNeighbourCountInMatrix(Position(1, 1), 2,
      """XX
        | X
        |   """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 2,
      """X X
        | X
        |   """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 2,
      """X
        |XX
        |   """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 2,
      """X
        | XX
        |   """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 2,
      """X
        | X
        |X  """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 2,
      """X
        | X
        | X  """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 2,
      """X
        | X
        |  X""")
  }

  it should "be 3" in {
    assertLiveNeighbourCountInMatrix(Position(1, 1), 3,
      """XXX
        | X
        |   """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 3,
      """XX
        |XX
        |   """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 3,
      """XX
        | XX
        |   """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 3,
      """XX
        | X
        |X  """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 3,
      """XX
        | X
        | X """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 3,
      """XX
        | X
        |  X""")
  }

  it should "be 4" in {
    assertLiveNeighbourCountInMatrix(Position(1, 1), 4,
      """XXX
        |XX
        |   """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 4,
      """XXX
        | XX
        |   """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 4,
      """XXX
        | X
        |X  """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 4,
      """XXX
        | X
        | X """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 4,
      """XXX
        | X
        |  X""")
  }

  it should "be 5" in {
    assertLiveNeighbourCountInMatrix(Position(1, 1), 5,
      """XXX
        |XXX
        |   """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 5,
      """XXX
        |XX
        |X  """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 5,
      """XXX
        |XX
        | X """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 5,
      """XXX
        |XX
        |  X""")
  }

  it should "be 6" in {
    assertLiveNeighbourCountInMatrix(Position(1, 1), 6,
      """XXX
        |XXX
        |X  """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 6,
      """XXX
        |XXX
        | X """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 6,
      """XXX
        |XXX
        |  X""")
  }

  it should "be 7" in {
    assertLiveNeighbourCountInMatrix(Position(1, 1), 7,
      """XXX
        |XXX
        |XX """)
    assertLiveNeighbourCountInMatrix(Position(1, 1), 7,
      """XXX
        |XXX
        |X X""")
  }

  it should "be 8" in {
    assertLiveNeighbourCountInMatrix(Position(1, 1), 8,
      """XXX
        |XXX
        |XXX""")
  }

}
