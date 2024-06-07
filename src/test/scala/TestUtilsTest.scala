import org.scalatest.flatspec.AnyFlatSpec

class TestUtilsTest extends AnyFlatSpec {

  "fromMatrix" should "return a set of 0 based row/column coordinates of non-space characters in a given string" in {
    assertResult(Set(Position(0, 1), Position(2, 3))) {
      TestUtils.fromMatrix(
        """ A
          |
          |   X""".stripMargin)
    }
  }

}
