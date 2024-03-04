package Q1Test
import a3q1.Q1

object Q1Test extends App {
  val q1Instance = new Q1()

  def testParents(): Unit = {
    // Test case 1: Existing entry with parentage information
    val input1 = "George"
    val expected1 = Right(("William", "Catherine"))
    val result1 = q1Instance.parents(input1)
    assert(result1 == expected1, s"Test case 1 failed: Expected $expected1, but got $result1")
    println(input1 + " parents: " + result1 + " expected: " + expected1)
    println("PASS")

    // Test case 2: Existing entry without parentage information
    val input2 = "Elizabeth"
    val expected2 = Left("Error: Elizabeth has no parent on the map")
    val result2 = q1Instance.parents(input2)
    assert(result2 == expected2, s"Test case 2 failed: Expected $expected2, but got $result2")
    println(input2 + "expected to has no parent on the map, got:\n" + result2)
    println("PASS")

    // Test case 3: Non-existing entry
    val input3 = "John"
    val expected3 = Left("Error: John has no entry on the map")
    val result3 = q1Instance.parents(input3)
    assert(result3 == expected3, s"Test case 3 failed: Expected $expected3, but got $result3")
    println(input3 + "expected to has no entry on the map, got:\n" + result3)
    println("PASS")

    println("All parents test cases passed!")
    println()
  }

  testParents()

  def testGrandparents(): Unit = {
    // Test case 1: Existing entry with grandparentage information
    val input1 = "George"
    val expected1 = Right(List("Diana", "Charles"))
    val result1 = q1Instance.grandparents(input1)
    assert(result1 == expected1, s"Test case 1 failed: Expected $expected1, but got $result1")
    println(input1 + " grandparents: " + result1 + " expected: " + expected1)
    println("PASS")

    // Test case 2: Existing entry without grandparentage information
    val input2 = "Elizabeth"
    val expected2 = Left("Error: Elizabeth has no grandparent on the map")
    val result2 = q1Instance.grandparents(input2)
    assert(result2 == expected2, s"Test case 2 failed: Expected $expected2, but got $result2")
    println(input2 + "expected to has no grandparent on the map, got:\n" + result2)
    println("PASS")

    // Test case 3: Non-existing entry
    val input3 = "John"
    val expected3 = Left("Error: John has no entry on the map")
    val result3 = q1Instance.grandparents(input3)
    assert(result3 == expected3, s"Test case 3 failed: Expected $expected3, but got $result3")
    println(input3 + "expected to has no entry on the map, got:\n" + result3)
    println("PASS")

    println("All grandparents test cases passed!")
    println()
  }

  testGrandparents()

  def testAunts(): Unit = {
    // Test case 1: Existing entry with aunt information
    val input1 = "George"
    val expected1 = Right(List("Megan"))
    val result1 = q1Instance.aunts(input1)
    assert(result1 == expected1, s"Test case 1 failed: Expected $expected1, but got $result1")
    println(input1 + " aunts: " + result1 + " expected: " + expected1)
    println("PASS")

    val input1_1 = "William"
    val expected1_1 = Right(List("Anne", "Sarah", "Sophie"))
    val result1_1 = q1Instance.aunts(input1_1)
    assert(result1_1 == expected1_1, s"Test case 1_1 failed: Expected $expected1_1, but got $result1_1")
    println(input1_1 + " aunts: " + result1_1 + " expected: " + expected1_1)
    println("PASS")

    // Test case 1.2: Existing with no aunts
    val input1_2 = "Edward"
    val expected1_2 = Right(List())
    val result1_2 = q1Instance.aunts(input1_2)
    assert(result1_2 == expected1_2, s"Test case 1_2 failed: Expected $expected1_2, but got $result1_2")
    println(input1_2 + " aunts: " + result1_2 + " expected: " + expected1_2)
    println("PASS")

    // Test case 2: Existing entry without parent information
    val input2 = "Elizabeth"
    val expected2 = Left("Error: Elizabeth has no parent on the map")
    val result2 = q1Instance.aunts(input2)
    assert(result2 == expected2, s"Test case 2 failed: Expected $expected2, but got $result2")
    println("PASS")

    // Test case 3: Non-existing entry
    val input3 = "John"
    val expected3 = Left("Error: John has no entry on the map")
    val result3 = q1Instance.aunts(input3)
    assert(result3 == expected3, s"Test case 3 failed: Expected $expected3, but got $result3")
    println("PASS")

    println("All aunts test cases passed!")
  }

  testAunts()

  def testFirstCousinsSpouses(): Unit = {
    // Test case 1: Existing entry with first cousin spouses
    val input1 = "William"
    val expected1 = Right(List("Edoardo", "Jack", "Mike", "Autumn"))
    val result1 = q1Instance.firstCousinsSpouses(input1)
    assert(result1 == expected1, s"Test case 1 failed: Expected $expected1, but got $result1")
    println(input1 + " first cousin spouses: " + result1 + " expected: " + expected1)
    println("PASS")

    val input1_1 = "Peter"
    val expected1_1 = Right(List("Edoardo", "Jack", "Catherine", "Meghan"))
    val result1_1 = q1Instance.firstCousinsSpouses(input1_1)
    assert(result1_1 == expected1_1, s"Test case 1_1 failed: Expected $expected1_1, but got $result1_1")
    println(input1_1 + " first cousin spouses: " + result1_1 + " expected: " + expected1_1)
    println("PASS")

    // Test case 1.2: Existing with no first cousin spouses
    val input1_2 = "George"
    val expected1_2 = Right(List())
    val result1_2 = q1Instance.firstCousinsSpouses(input1_2)
    assert(result1_2 == expected1_2, s"Test case 1_2 failed: Expected $expected1_2, but got $result1_2")
    println(input1_2 + " first cousin spouses: " + result1_2 + " expected: " + expected1_2)
    println("PASS")

    // Test case 2: Existing entry without first cousin spouses
    val input2 = "Elizabeth"
    val expected2 = Left("Error: Elizabeth has no parent on the map")
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