package fibActor
import akka.actor._
import akka.util.Timeout
import akka.pattern.ask
import scala.concurrent.Await
import scala.concurrent.duration._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.ExecutionContext.Implicits.global

class FibActorTest extends AnyFlatSpec with Matchers {
  implicit val timeout: Timeout = Timeout(50.hours)

  "FibActor" should "correctly compute Fibonacci numbers" in {
    val system = ActorSystem("TestSystem")
    val fibActor = system.actorOf(FibActor.props())

    val testCases: List[(Int, Int)] = List(
      /* (0, 0), (1, 1), */ (2, 1), (3, 2), (4, 3), (5, 5),
      (6, 8), (7, 13), (8, 21), (9, 34), (10, 55),
      (11, 89), (12, 144), (13, 233), (14, 377), 
      (15, 610), (16, 987), (17, 1597), (18, 2584),
      (19, 4181), (20, 6765), (21, 10946), (22, 17711),
      (23, 28657), (24, 46368), (25, 75025), (26, 121393), (27, 196418), 
      (28, 317811), (29, 514229), (30, 832040), (31, 1346269), (32, 217830)
    )

    testCases.foreach { case (n, expected) =>
      println(s"Testing Fib($n), expecting result: $expected")
      val future = (fibActor ? FibActor.Req(n)).mapTo[FibActor.Res]
      val FibActor.Res(result) = Await.result(future, timeout.duration)
      println(s"Result for Fib($n): $result, expected: $expected")
      result should be(expected)
      println(s"Test case length for Fib($n): ${n.toString.length}")
      println()
    }

    system.terminate()
  }
}
