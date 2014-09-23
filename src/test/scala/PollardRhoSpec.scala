import org.scalatest._

import scala.collection.Bag

class PollardRhoSpec  extends FlatSpec with Matchers with OptionValues with Inside with Inspectors {
  import PollardRho._

  "The pollardRho" should "factor numbers correctly" in {
    factor(2) should be (Bag(BigInt(2)))
    factor(3) should be (Bag(BigInt(3)))
    factor(9) should be (Bag(BigInt(3),BigInt(3)))
    factor(10) should be (Bag(BigInt(2),BigInt(5)))
    factor(8616460799L) should be (Bag(BigInt(89681), BigInt(96079)))
  }

  "The pollardRho" should "calculate proper divisors correctly" in {
    sumProperDivisors(3) should be (1)
    sumProperDivisors(4) should be (1 + 2)
    sumProperDivisors(1800) should be (6045)
    sumProperDivisors(12) should be (1 + 2 + 3 + 4 + 6)
  }
}
