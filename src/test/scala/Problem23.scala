import org.scalatest._

import scala.collection.BitSet
import breeze.linalg._

class Problem23  extends FlatSpec with Matchers with OptionValues with Inside with Inspectors {
  "Problem 23" should "be solved" in {
    import PollardRho._

    def isAbundant(i: Int) = sumProperDivisors(i) > i

    val max = 28123
    val abundants = BitSet.empty ++ (1 to max) filter isAbundant
    val sums = for (ab1 ← abundants; ab2 ← abundants.from(ab1) if (ab1 < ab2 && ab1 + ab2 < max))
    yield ab1 + ab2

    val all = BitSet.empty ++ (1 to max)
    val notInSet = all -- sums
    println(notInSet.sum)
  }
}
