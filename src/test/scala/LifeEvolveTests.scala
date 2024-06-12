import TestUtils.fromMatrix
import org.scalatest.flatspec.AnyFlatSpec

class LifeEvolveTests extends AnyFlatSpec {

  private def assertEvolve(cycles: Int, from: String, to: String): Any =
    val life = Life(fromMatrix(from))
    assertResult(fromMatrix(to)) {
      life.evolve(cycles)
      life.liveCells
    }

  "live cell with 0 neighbours" should "die" in {
    assertEvolve(1,
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
    assertEvolve(1,
      """|1
         | 1
         |
      """, """""")
    assertEvolve(1,
      """| 1
         | 1
         |
      """, """""")
    assertEvolve(1,
      """|  1
         | 1
         |
      """, """""")
    assertEvolve(1,
      """|
         |11
         |
      """, """""")
  }

  "live cell with 2 neighbours" should "stay alive" in {
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
      """|nn
         |
         |
      """,
      """|
         |
         |
      """)
    assertEvolve(1,
      """|n n
         |
         |
      """,
      """|
         |
         |
      """)
    assertEvolve(1,
      """|n
         |n
         |
      """,
      """|
         |
         |
      """)
    assertEvolve(1,
      """|n
         |  n
         |
      """,
      """|
         |
         |
      """)
    assertEvolve(1,
      """|n
         |
         |n
      """,
      """|
         |
         |
      """)
    assertEvolve(1,
      """|n
         |
         | n
      """,
      """|
         |
         |
      """)
    assertEvolve(1,
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
    assertEvolve(1,
      """|
         |X3X
         | 3
      """,
      """| +
         |XXX
         |+X+
      """)
    assertEvolve(1,
      """|
         |33
         |33
      """,
      """|
         |XX
         |XX
      """)
    assertEvolve(1,
      """|
         |X3
         | 3X
      """,
      """|
         |XX+
         |+XX
      """)
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
      """|
         |nn
         |n
      """,
      """|
         |XX
         |XX
      """)
    assertEvolve(1,
      """|
         |nn
         |  n
      """,
      """|
         | X
         | X
      """)
    assertEvolve(1,
      """|
         |nn
         |
         |n
      """,
      """|
         |
         |++
      """)
    assertEvolve(1,
      """|
         |nn
         |
         | n
      """,
      """|
         |
         |++
      """)
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
      """|
         |x4x
         |x4
      """,
      """| +
         |x x
         |x +
      """)
    assertEvolve(1,
      """|
         |x4x
         | 4x
      """,
      """| +
         |x x
         |+ x
      """)
    assertEvolve(1,
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
    assertEvolve(1,
      """|
         |xxx
         | 4
         | x
      """,
      """| +
         |xxx
         |
      """)
    assertEvolve(1,
      """|
         |xxx
         | 4
         |  x
      """,
      """| +
         |xxx
         |+
      """)
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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
    assertEvolve(1,
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

  "2 cycles" should "take us further" in {
    assertEvolve(2,
      """|
         | XXX
         | X7X
         | XXX
      """,
      """|  +
         | X+X
         |X+ +X
         | X+X
         |  X
      """)
  }


}
