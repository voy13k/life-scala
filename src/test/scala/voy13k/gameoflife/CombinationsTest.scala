package voy13k.gameoflife

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.*

class CombinationsTest extends AnyFlatSpecLike with Matchers {

  "1 element combinations" must "match" in {
    val combinations = Combinations.allNCombinations(1, Set("A", "B", "C", "D", "E"))
    combinations shouldBe Set(
      Set("A"),
      Set("B"),
      Set("B"),
      Set("C"),
      Set("D"),
      Set("E"),
    )
  }

  "2 element combinations" must "match" in {
    val combinations = Combinations.allNCombinations(2, Set("A", "B", "C", "D", "E"))
    combinations shouldBe Set(
      Set("A", "B"),
      Set("A", "C"),
      Set("A", "D"),
      Set("A", "E"),
      Set("B", "C"),
      Set("B", "D"),
      Set("B", "E"),
      Set("C", "D"),
      Set("C", "E"),
      Set("D", "E"),
    )
  }

  "3 element combinations" must "match" in {
    val combinations = Combinations.allNCombinations(3, Set("A", "B", "C", "D", "E"))
    combinations shouldBe Set(
      Set("A", "B", "C"),
      Set("A", "B", "D"),
      Set("A", "B", "E"),
      Set("A", "C", "D"),
      Set("A", "C", "E"),
      Set("A", "D", "E"),
      Set("B", "C", "D"),
      Set("B", "C", "E"),
      Set("B", "D", "E"),
      Set("C", "D", "E"),
    )
  }

  "4 element combinations" must "match" in {
    val combinations = Combinations.allNCombinations(4, Set("A", "B", "C", "D", "E"))
    combinations shouldBe Set(
      Set("A", "B", "C", "D"),
      Set("A", "B", "C", "E"),
      Set("A", "B", "D", "E"),
      Set("A", "C", "D", "E"),
      Set("B", "C", "D", "E"),
    )
  }

  "5 element combinations" must "match" in {
    val combinations = Combinations.allNCombinations(5, Set("A", "B", "C", "D", "E"))
    combinations shouldBe Set(
      Set("A", "B", "C", "D", "E"),
    )
  }
}
