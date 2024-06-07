import TestUtils.fromMatrix
import org.scalatest.flatspec.AnyFlatSpec

class LifeTickTests extends AnyFlatSpec {

  private def assertTick(from: String, to: String) =
    assertResult(fromMatrix(to.stripMargin)) {
      tick(fromMatrix(from.stripMargin))
    }

  "live cell with 0 neighbours" should "die" in {
    assertTick(
      """|
         | 0
         |
      """,
      """|
         |
         |
      """)
  }

  "live cell with 1 neighbour" should "die" in {
    assertTick(
      """|1
         | 1
         |
      """,
      """""")
    assertTick(
      """| 1
         | 1
         |
      """,
      """""")
    assertTick(
      """|  1
         | 1
         |
      """,
      """""")
    assertTick(
      """|
         |11
         |
      """,
      """""")
  }

  "live cell with 2 neighbours" should "stay alive" in {
    assertTick(
      """|
         | 22
         |  2
         |
      """,
      """|
         | XX
         | XX
         |
      """)
    assertTick(
      """|
         | X X
         |  2
         |
      """,
      """|
         |  X
         |  X
         |
      """)
    assertTick(
      """|
         | 2
         | 22
         |
      """,
      """|
         | XX
         | XX
         |
      """)
    assertTick(
      """|
         | X
         |  2X
         |
      """,
      """|
         |  X
         |  X
         |
      """)
    assertTick(
      """|
         | X
         |  2
         | X
      """,
      """|
         |
         | XX
         |
      """)
    assertTick(
      """|
         | X
         |  2
         |  X
      """,
      """|
         |
         | XX
         |
      """)
    assertTick(
      """|
         | X
         |  2
         |   X
      """,
      """|
         |
         |  X
         |
      """)
  }

  "dead cell with 2 neighbours" should "stay dead" in {
    assertTick(
      """|nn
         |
         |
      """,
      """|
         |
         |
      """)
    assertTick(
      """|n n
         |
         |
      """,
      """|
         |
         |
      """)
    assertTick(
      """|n
         |n
         |
      """,
      """|
         |
         |
      """)
    assertTick(
      """|n
         |  n
         |
      """,
      """|
         |
         |
      """)
    assertTick(
      """|n
         |
         |n
      """,
      """|
         |
         |
      """)
    assertTick(
      """|n
         |
         | n
      """,
      """|
         |
         |
      """)
    assertTick(
      """|n
         |
         |  n
      """,
      """|
         |
         |
      """)
  }

  "live cell with 3 neighbours" should "stay alive" in {
    assertTick(
      """|
         |X3X
         | 3
      """,
      """| +
         |XXX
         |+X+
      """)
    assertTick(
      """|
         |33
         |33
      """,
      """|
         |XX
         |XX
      """)
    assertTick(
      """|
         |X3
         | 3X
      """,
      """|
         |XX+
         |+XX
      """)
    assertTick(
      """|
         |XX
         | 3
         |X
      """,
      """|
         |XX
         | X
         |
      """)
    assertTick(
      """|
         |XX
         | 3
         | X
      """,
      """|
         |XX
         | X+
         |
      """)
    assertTick(
      """|
         |XX
         | 3
         |  X
      """,
      """|
         |XX
         |+X+
         |
      """)
  }

  "dead cell with 3 neighbours" should "become alive" in {
    assertTick(
      """|
         |nnn
         |
         |
      """,
      """| +
         | X
         | +
         |
      """)
    assertTick(
      """|
         |nn
         |n
      """,
      """|
         |XX
         |XX
      """)
    assertTick(
      """|
         |nn
         |  n
      """,
      """|
         | X
         | X
      """)
    assertTick(
      """|
         |nn
         |
         |n
      """,
      """|
         |
         |++
      """)
    assertTick(
      """|
         |nn
         |
         | n
      """,
      """|
         |
         |++
      """)
    assertTick(
      """|
         |nn
         |
         |  n
      """,
      """|
         |
         | +
         |
      """)
    assertTick(
      """|
         |n n
         |
         |n
      """,
      """|
         |
         | +
         |
      """)
    assertTick(
      """|
         |n n
         |
         | n
      """,
      """|
         |
         | +
         |
      """)
    assertTick(
      """|
         |n
         |  n
         | n
      """,
      """|
         |
         | +
         |
      """)
  }

  "cell with 4 neighbours" should "die" in {
    assertTick(
      """|
         |x4x
         |x4
      """,
      """| +
         |x x
         |x +
      """)
    assertTick(
      """|
         |x4x
         | 4x
      """,
      """| +
         |x x
         |+ x
      """)
    assertTick(
      """|
         |xxx
         | 4
         |x
      """,
      """| +
         |xxx
         |  +
         |
      """)
    assertTick(
      """|
         |xxx
         | 4
         | x
      """,
      """| +
         |xxx
         |
      """)
    assertTick(
      """|
         |xxx
         | 4
         |  x
      """,
      """| +
         |xxx
         |+
      """)
    assertTick(
      """|
         |x x
         | 4
         |x x
      """,
      """|
         | +
         |+ +
         | +
      """)
    assertTick(
      """|
         | x
         |x4x
         | x
      """,
      """|
         |+X+
         |X X
         |+X+
      """)
  }

  "live cell with 5 neighbours" should "die" in {
    assertTick(
      """|
         |X5X
         |X5X
         |
      """,
      """| +
         |X X
         |X X
         | +
      """)
    assertTick(
      """|
         | XXX
         | X5
         | X
      """,
      """|  +
         | X X
         |+  +
         | X+
      """)
    assertTick(
      """|
         | XXX
         | X5
         |  X
      """,
      """|  +
         | X X
         |
         | +X
      """)
    assertTick(
      """|
         | XXX
         | X5
         |   X
      """,
      """|  +
         | X X
         | X
         |  +
      """)
    assertTick(
      """|
         | XXX
         |  5
         | X X
      """,
      """|  +
         | XXX
         |
         |  +
      """)
    assertTick(
      """|
         | XXX
         |  5
         | XX
      """,
      """|  +
         | XXX
         |
         | XX
      """)
    assertTick(
      """|
         |  XX
         | X5
         | XX
      """,
      """|
         | +XX
         |
         | XX
      """)
    assertTick(
      """|
         | XX
         |  5X
         | XX
      """,
      """|
         | XX+
         |   X
         | XX+
      """)
  }

  "live cell with 6 neighbours" should "die" in {
    assertTick(
      """|
         | XXX
         | X6X
         | X
      """,
      """|  +
         | X X
         |+  X
         | X
      """)
    assertTick(
      """|
         | XXX
         | X6X
         |  X
      """,
      """|  +
         | X X
         |
         | +X+
      """)
    assertTick(
      """|
         | XXX
         |  6X
         | XX
      """,
      """|  +
         | X X
         |
         | XX+
      """)
    assertTick(
      """|
         |  XX
         | X6X
         | XX
      """,
      """|
         | + X
         |
         | X +
      """)
    assertTick(
      """|
         | X X
         | X6X
         | XX
      """,
      """|
         | X X
         |+  X
         | X +
      """)
    assertTick(
      """|
         | X X
         | X6X
         | X X
      """,
      """|
         | X X
         |+X X+
         | X X
      """)
  }

  "live cell with 7 neighbours" should "die" in {
    assertTick(
      """|
         | XXX
         | X7X
         | XX
      """,
      """|  +
         | X X
         |+
         | X +
      """)
    assertTick(
      """|
         | XXX
         | X7X
         | X X
      """,
      """|  +
         | X X
         |+   +
         | X X
      """)
  }

  "live cell with 8 neighbours" should "die" in {
    assertTick(
      """|
         | XXX
         | X7X
         | XXX
      """,
      """|  +
         | X X
         |+   +
         | X X
         |  +
      """)
  }

}
