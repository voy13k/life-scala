import org.scalatest.Inspectors.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

import java.io.ByteArrayOutputStream

class AppTests extends AnyFlatSpec with Matchers {

  "Two cells" should "die off completely" in {
    assertLife("[[3, 1], [3,2]]", Set())
  }

  "Glider" should "continue gliding" in {
    assertLife("[[3, 1], [3, 2], [3, 3], [2, 3], [1, 2]]", Set(
      "[28, 28]", "[26, 27]", "[28, 27]", "[27, 28]", "[28, 26]"
    ))
  }

  private def assertLife(seed: String, expected: Set[String]) =
    val capturedStdOut = ByteArrayOutputStream()
    Console.withOut(capturedStdOut) {
      app(seed)
    }
    val appOutput = capturedStdOut.toString.lines.toList.getLast.split("\\s*:")
    val (cycle, livingCells) = (appOutput(0), appOutput(1))

    cycle should be("100")
    forAll(expected) { e => livingCells should include(e) }
    livingCells.count(_ == ',') should be(if expected.isEmpty then 0 else expected.size * 2 - 1)

}
