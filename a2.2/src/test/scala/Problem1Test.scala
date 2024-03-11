package q1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Problem1Test extends AnyFlatSpec with Matchers {
  "iterateUsingUnfold" should "generate the correct LazyList" in {
    val result = Problem1.iterateUsingUnfold((i: Int) => i + 1)(1).take(5).toList
    assert(result == List(1, 2, 3, 4, 5))
    println(s"iterateUsingUnfold example: $result")
  }

  "iterateWithoutUnfold" should "generate the correct LazyList" in {
    val result = Problem1.iterateWithoutUnfold((i: Int) => i * 2)(1).take(5).toList
    assert(result == List(1, 2, 4, 8, 16))
    println(s"iterateWithoutUnfold example: $result")
  }

  "iterateWithoutUnfold and iterateUsingUnfold" should 
  "generate the same LazyList 1" in {
    val result1 = 
      Problem1.iterateUsingUnfold((i: Int) => i + 1)(1).take(10).toList
    val result2 = 
      Problem1.iterateWithoutUnfold((i: Int) => i + 1)(1).take(10).toList
    assert(result1 == result2)
    println(s"iterateUsingUnfold and iterateWithoutUnfold example: $result1")
  }

  "iterateWithoutUnfold and iterateUsingUnfold" should 
  "generate the same LazyList 2" in {
    val result1 = 
      Problem1.iterateUsingUnfold((i: Int) => i * 2)(13).take(30).toList
    val result2 = 
      Problem1.iterateWithoutUnfold((i: Int) => i * 2)(13).take(30).toList
    assert(result1 == result2)
    println(s"iterateUsingUnfold and iterateWithoutUnfold example: $result1")
  }


}
