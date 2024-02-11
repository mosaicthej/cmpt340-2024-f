import org.scalatest.flatspec.AnyFlatSpec
import q3.Q3

class Q3Spec extends AnyFlatSpec {
  val q3Instance = new Q3()

  "int2bin" should "correctly convert integers to binary" in {
    val testNumbers = List((0, List(0)), (1, List(1)), (2, List(1, 0)), (3, List(1, 1)), (15, List(1, 1, 1, 1)))
    testNumbers.foreach { case (number, expected) =>
      assert(q3Instance.int2bin(number) == expected, s"Failed for $number")
    }
  }

  "repHalve" should "correctly halve lists repeatedly" in {
    val testList = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
    val expectedResult = List(List(1, 2, 3, 4, 5, 6, 7, 8), List(9, 10, 11, 12), List(13, 14), List(15))
    assert(q3Instance.repHalve(testList) == expectedResult)
  }
}
