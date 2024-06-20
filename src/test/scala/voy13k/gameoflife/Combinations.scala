package voy13k.gameoflife

object Combinations:

  def allNCombinations[E](n: Int, set: Set[E]): Set[Set[E]] =
    if n == 0 then Set(Set())
    else
      set.flatMap(e =>
        val withoutE = set - e
        // Part2: n long subsets without e
        val eLessSubset = allNCombinations(n, withoutE)
        // Part1: (n-1) long subsets without e, each recombined with e
        val eSubset = allNCombinations(n - 1, withoutE).map(subCombo => subCombo + e)
        eSubset ++ eLessSubset
      )

