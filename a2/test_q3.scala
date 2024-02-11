package Q3Test
import q3.Q3

object Q3Test extends App {
  val q3Instance = new Q3()

  // Test for int2bin
  def testInt2Bin(): Unit = {
    println("Testing int2bin:")
    val testNumbers = List(0, 1, 2, 3, 4, 5, 15, 1023)
    testNumbers.foreach { number =>
      println(s"$number in binary is ${q3Instance.int2bin(number)}")
    }
  }

  // Test for repHalve
  def testRepHalve(): Unit = {
    println("Testing repHalve:")
    val testList = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
    println(s"Input list: $testList")
    println(s"Result: ${q3Instance.repHalve(testList)}")
  }

  // Run tests
  testInt2Bin()
  testRepHalve()
}
