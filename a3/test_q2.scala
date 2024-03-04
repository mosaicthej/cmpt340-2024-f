package Q2Test
import a3q2.Partial
import scala.util.{Success=>_, Failure=>_, _}

object Q2Test extends App {
  val partialInstance = new Partial()

  // Test map function
  val mapTest = Success(5).map(_ * 2)
  assert(mapTest == Success(10), s"mapTest failed: Expected Success(10), but got $mapTest")
  println("mapTest passed!, got: Success(10)")

  // Test flatMap function
  val flatMapTest = Success(5).flatMap(x => Success(x * 2))
  assert(flatMapTest == Success(10), s"flatMapTest failed: Expected Success(10), but got $flatMapTest")
  println("flatMapTest passed!, got: Success(10)")

  // Test getOrElse function
  val getOrElseTest1 = Success(5).getOrElse(0)
  assert(getOrElseTest1 == 5, s"getOrElseTest1 failed: Expected 5, but got $getOrElseTest1")
  println("getOrElseTest1 passed!, got: 5")

  val getOrElseTest2 = Errors(Seq("Error")).getOrElse(0)
  assert(getOrElseTest2 == 0, s"getOrElseTest2 failed: Expected 0, but got $getOrElseTest2")
  println("getOrElseTest2 passed!, got: 0")

  // Test orElse function
  val orElseTest1 = Success(5).orElse(Success(10))
  assert(orElseTest1 == Success(5), s"orElseTest1 failed: Expected Success(5), but got $orElseTest1")
  println("orElseTest1 passed!, got: Success(5)")

  val orElseTest2 = Errors(Seq("Error")).orElse(Success(10))
  assert(orElseTest2 == Success(10), s"orElseTest2 failed: Expected Success(10), but got $orElseTest2")
  println("orElseTest2 passed!, got: Success(10)")

  // Test map3 function
  val map3Test = Success(2).map3(Success(3), Success(4))((a, b, c) => a + b + c)
  assert(map3Test == Success(9), s"map3Test failed: Expected Success(9), but got $map3Test")
  println("map3Test passed!, got: Success(9)")

  // Test map2 function
  val map2Test = Success(2).map2(Success(3))((a, b) => a + b)
  assert(map2Test == Success(5), s"map2Test failed: Expected Success(5), but got $map2Test")
  println("map2Test passed!, got: Success(5)")

  // Test sequence function
  val sequenceTest1 = Partial.sequence(List(Success(1), Success(2), Success(3)))
  assert(sequenceTest1 == Success(List(1, 2, 3)), s"sequenceTest1 failed: Expected Success(List(1, 2, 3)), but got $sequenceTest1")
  println("sequenceTest1 passed!, got: Success(List(1, 2, 3))"

  val sequenceTest2 = Partial.sequence(List(Success(1), Errors(Seq("Error")), Success(3)))
  assert(sequenceTest2 == Errors(Seq("Error")), s"sequenceTest2 failed: Expected Errors(Seq(\"Error\")), but got $sequenceTest2")
  println("sequenceTest2 passed!, got: Errors(Seq(\"Error\"))")

  println("All tests passed!")
}