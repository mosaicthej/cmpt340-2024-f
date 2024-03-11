package q2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Q2Test extends AnyFlatSpec with Matchers {

  val primeGenerator = new Q2()

  "unfold" should "correctly generate sequences" in {
    val sequence = primeGenerator.unfold(0) {
      case n if n < 5 => Some((n, n + 1))
      case _ => None
    }.toList
    sequence shouldEqual List(0, 1, 2, 3, 4)
  }

  "iterateUsingUnfold" should "generate an incremental sequence" in {
    val incremental = primeGenerator.iterateUsingUnfold[Int](_ + 1)(1).take(5).toList
    incremental shouldEqual List(1, 2, 3, 4, 5)
  }

  "iterateWithoutUnfold" should "generate an incremental sequence" in {
    val incremental = primeGenerator.iterateWithoutUnfold[Int](_ + 1)(1).take(5).toList
    incremental shouldEqual List(1, 2, 3, 4, 5)
  }

  "isCoPrimeWith" should "determine coprimality correctly" in {
    val primes = primeGenerator.primes.take(10) // Take the first 10 primes to test against
    primeGenerator.isCoPrimeWith(primes, 11) shouldBe true
    primeGenerator.isCoPrimeWith(primes, 4) shouldBe false
  }

  "primes" should "generate the correct sequence of prime numbers" in {
    val primes = primeGenerator.primes.take(10).toList
    primes shouldEqual List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
  }

  "primesWithUnfold" should "generate the correct sequence of prime numbers" in {
    val primesWithUnfold = primeGenerator.primesWithUnfold.take(10).toList
    primesWithUnfold shouldEqual List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
  }

  "primesWithoutUnfold" should "generate the correct sequence of prime numbers" in {
    val primesWithoutUnfold = primeGenerator.primesWithoutUnfold.take(10).toList
    primesWithoutUnfold shouldEqual List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
  }

}

