package q3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Q3Test extends AnyFlatSpec with Matchers {

  val tripleGenerator = new Q3()

  "pyth" should "generate correct Pythagorean triples up to a limit" in {
    val limit = 20
    val expectedTriples = 
      Set((3, 4, 5), (5, 12, 13), (6, 8, 10), (8, 15, 17), 
        (9, 12, 15), (12, 16, 20), (16,12,20), (8,6,10), (12,5,13), 
        (12,9,15), (4,3,5), (15,8,17))
    val triples = tripleGenerator.pyth(limit).toSet

    triples shouldEqual expectedTriples
  }

  "pythUnfold" should 
  "generate correct Pythagorean triples up to a limit using unfold" in {
    val limit = 20
    val expectedTriples = Set((3, 4, 5), (5, 12, 13), (6, 8, 10), 
      (8, 15, 17), (9, 12, 15), (12, 16, 20), (16,12,20), (8,6,10), 
      (12,5,13), (12,9,15), (4,3,5), (15,8,17))
    val triples = tripleGenerator.pythUnfold(limit).toSet

    triples shouldEqual expectedTriples
  }
}
