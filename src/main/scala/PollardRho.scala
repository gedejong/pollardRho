import java.math.BigInteger
import java.security.SecureRandom

import rx.lang.scala.Observable

import scala.BigInt._
import scala.annotation.tailrec
import scala.collection.{immutable, Bag}

object PollardRho {
  private final val random = new SecureRandom

  def rho(n: BigInt): BigInt = {

    if (n % 2 == 0)
      2
    else {

      val c = BigInt(n.bitLength, random)
      var x = BigInt(n.bitLength, random)
      var xx = x
      var divisor: BigInt = 1

      do {
        x = ((x ^ 2 % n) + c) % n
        xx = ((xx ^ 2 % n) + c) % n
        xx = ((xx ^ 2 % n) + c) % n
        divisor = (x - xx) gcd n
      } while (divisor == 1)

      divisor
    }
  }

  implicit val bagConfiguration = Bag.configuration.compact[BigInt]

  /** Non tailrecursive, should refactor */
  def factor(n: BigInt): Bag[BigInt] = {
    if (n == 1) Bag()
    else if (n.isProbablePrime(20)) Bag(n)
    else {
      val divisor = rho(n)
      factor(divisor) ++ factor(n / divisor)
    }
  }

  def sumDivisors(n: BigInt): BigInt = {
    val divisors = for ((f, m) ← factor(n).multiplicities) yield
      ((f ^ (m + 1)) - 1) / (f - 1)

    divisors.product
  }

  def sumProperDivisors(n: BigInt): BigInt = {
    val factors = factor(n)
    val divisors = for ((f, m) ← factors.multiplicities) yield
      ((f ^ (m + 1)) - 1) / (f - 1)

    divisors.product - factors.product
  }

  implicit class PollardRhoPimpedBigInt(value: BigInt) {
    def factor: Bag[BigInt] = PollardRho.factor(value)
    def sumDivisors: BigInt = PollardRho.sumDivisors(value)
    def sumProperDivisors: BigInt = PollardRho.sumProperDivisors(value)
  }

  def rho2(n: BigInt): BigInt = {
    def genRand = BigInt(n.bitLength, random)
    val c = genRand

    @inline def fn(x: BigInt) = ((x ^ 2 % n) + c) % n

    val guessStream = for {
      x <- Stream.iterate(genRand)(fn)
      xx <- Stream.iterate(genRand)(x ⇒ fn(fn(x)))
    } yield (x - xx) gcd n

    if (n % 2 == 0) 2 else guessStream.filter(_ != 1).head
  }
}
