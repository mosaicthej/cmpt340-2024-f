import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Problem1Test extends AnyFlatSpec with Matchers {
  "iterateUsingUnfold" should "generate an infinite LazyList starting with 2 and incrementing by 3" in {
    Problem1.iterateUsingUnfold((i: Int) => i + 3)(2).take(4).toList shouldEqual List(2, 5, 8, 11)
  }

  "iterateWithoutUnfold" should "generate an infinite LazyList starting with 2 and incrementing by 3" in {
    Problem1.iterateWithoutUnfold((i: Int) => i + 3)(2).take(4).toList shouldEqual List(2, 5, 8, 11)
  }
}
