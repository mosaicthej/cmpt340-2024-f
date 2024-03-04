package Q1Test
import q1.Q1

object Q1Test extends App {
  val q1Instance = new Q1()

  def testParents(): Unit = {
    // Test case 1: Existing entry with parentage information
    val input1 = "George"
    val expected1 = Right(("William", "Catherine"))
    val result1 = q1Instance.parents(input1)
    assert(result1 == expected1, s"Test case 1 failed: Expected $expected1, but got $result1")

    // Test case 2: Existing entry without parentage information
    val input2 = "Elizabeth"
    val expected2 = Left("Error: Elizabeth has no parent on the map")
    val result2 = q1Instance.parents(input2)
    assert(result2 == expected2, s"Test case 2 failed: Expected $expected2, but got $result2")

    // Test case 3: Non-existing entry
    val input3 = "John"
    val expected3 = Left("Error: John has no entry on the map")
    val result3 = q1Instance.parents(input3)
    assert(result3 == expected3, s"Test case 3 failed: Expected $expected3, but got $result3")

    println("All parents test cases passed!")
  }

  testParents()

  def testGrandparents(): Unit = {
    // Test case 1: Existing entry with grandparentage information
    val input1 = "George"
    val expected1 = Right(List("Diana", "Charles", "Elizabeth", "Philip"))
    val result1 = q1Instance.grandparents(input1)
    assert(result1 == expected1, s"Test case 1 failed: Expected $expected1, but got $result1")

    // Test case 2: Existing entry without grandparentage information
    val input2 = "Elizabeth"
    val expected2 = Left("Error: Elizabeth has no grandparent on the map")
    val result2 = q1Instance.grandparents(input2)
    assert(result2 == expected2, s"Test case 2 failed: Expected $expected2, but got $result2")

    // Test case 3: Non-existing entry
    val input3 = "John"
    val expected3 = Left("Error: John has no entry on the map")
    val result3 = q1Instance.grandparents(input3)
    assert(result3 == expected3, s"Test case 3 failed: Expected $expected3, but got $result3")

    println("All grandparents test cases passed!")
  }

  testGrandparents()

  def testAunts(): Unit = {
    // Test case 1: Existing entry with aunt information
    val input1 = "George"
    val expected1 = Right(List("Beatrice", "Eugenie"))
    val result1 = q1Instance.aunts(input1)
    assert(result1 == expected1, s"Test case 1 failed: Expected $expected1, but got $result1")

    // Test case 2: Existing entry without aunt information
    val input2 = "Elizabeth"
    val expected2 = Left("Error: Elizabeth has no aunt on the map")
    val result2 = q1Instance.aunts(input2)
    assert(result2 == expected2, s"Test case 2 failed: Expected $expected2, but got $result2")

    // Test case 3: Non-existing entry
    val input3 = "John"
    val expected3 = Left("Error: John has no entry on the map")
    val result3 = q1Instance.aunts(input3)
    assert(result3 == expected3, s"Test case 3 failed: Expected $expected3, but got $result3")

    println("All aunts test cases passed!")
  }

  testAunts()

  def testFirstCousinsSpouses(): Unit = {
    // Test case 1: Existing entry with first cousin spouses
    val input1 = "George"
    val expected1 = Right(List("Edoardo", "Jack"))
    val result1 = q1Instance.firstCousinsSpouses(input1)
    assert(result1 == expected1, s"Test case 1 failed: Expected $expected1, but got $result1")

    // Test case 2: Existing entry without first cousin spouses
    val input2 = "Elizabeth"
    val expected2 = Left("Error: Elizabeth has no first cousin spouse on the map")
    val result2 = q1Instance.firstCousinsSpouses(input2)
    assert(result2 == expected2, s"Test case 2 failed: Expected $expected2, but got $result2")

    // Test case 3: Non-existing entry
    val input3 = "John"
    val expected3 = Left("Error: John has no entry on the map")
    val result3 = q1Instance.firstCousinsSpouses(input3)
    assert(result3 == expected3, s"Test case 3 failed: Expected $expected3, but got $result3")

    println("All first cousins spouses test cases passed!")
  }

  testFirstCousinsSpouses()
}